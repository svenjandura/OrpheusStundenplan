{-# LANGUAGE Arrows #-}

module Parser where

import Seminar
import Config

import Text.XML.HXT.Core
import Data.Maybe
import Data.Tree.NTree.TypeDefs
import GHC.Exts

-- (Thema-ID, User-ID, Wahl)
type ThemenwahlIndex = (Nid, Int, Int)

leseSeminar :: Config -> IO Seminar
leseSeminar config = do
  let dir = cWebsiteDaten config
  zeiteinheiten <-parseToList (dir ++ "zeiteinheiten.xml") "nodes" "node" parseZeiteinheit
  let sortedZeiteinheiten = sortWith zzeitspanne zeiteinheiten
  raueme <- parseToList (dir ++ "räume.xml") "nodes" "node" parseRaum
  themen <- parseToList (dir ++ "themenauswahl.xml") "nodes" "node" parseThema
  themenwahlen <- parseToList (dir ++ "themenwahlen.xml") "nodes" "node" parseThemenwahl
  teilnehmer <- parseToList (dir ++ "teilnehmer-und-betreuer.xml") "users" "user" (parseTeilnehmer themenwahlen themen)
  betreuer <- parseToList (dir ++ "teilnehmer-und-betreuer.xml") "users" "user" (parseBetreuer themenwahlen themen)
  zuweisungen <- parseToList (cZwangszuweisungen config) "zuweisungen" "zuweisung" (parseZuweisung themen teilnehmer)
  voraussetzungen <- parseToList (dir ++ "alle-voraussetzungen.xml") "eck_voraussetzungs" "eck_voraussetzung" (parseVoraussetzung themen)
  return $ Seminar{ szeiteinheiten = sortedZeiteinheiten
                  , sraueme = raueme
                  , sthemen = themen
                  , steilnehmer = teilnehmer
                  , sbetreuer = betreuer
                  , szwangszuweisungen = zuweisungen
                  , svoraussetzungen = voraussetzungen}

leseGlobalplan :: Seminar -> String -> IO Globalplan
leseGlobalplan seminar file = do
  planeinheitenStr <- parseToList file "einheiten" "vortrag" parsePlaneinheit
  let planeinheiten = map (matchPlaneinheit seminar) planeinheitenStr
  return $ Globalplan seminar $ indexPlaneinheiten planeinheiten

matchPlaneinheit :: Seminar -> (Int, String, String, String) -> Planeinheit
matchPlaneinheit seminar (einheit, themaStr, betreuerStr, raumStr) =
  let zeiteinheit = (filter istPEOderExk $ szeiteinheiten seminar) !! (einheit -1)
      thema = findeByNameError "Thema" (sthemen seminar) themaStr
      betreuer = findeByNameError "Betreuer" (sbetreuer seminar) betreuerStr
      raum = findeByNameError "Raum" (sraueme seminar) raumStr
  in Planeinheit (Nid 0) zeiteinheit thema betreuer raum

indexPlaneinheiten :: [Planeinheit] -> [Planeinheit]
indexPlaneinheiten [p] = [ p{ pid = Nid 0} ]
indexPlaneinheiten (p:ps) =
  let prev = indexPlaneinheiten ps
      pNeu = p {pid = Nid $ toInteger (length ps) }
  in (pNeu:prev)

increaseNid :: Nid -> Nid
increaseNid (Nid i) = Nid (i+1)


parseXML :: String ->  IOStateArrow s b XmlTree
parseXML file = readDocument [ withValidate no
                               , withRemoveWS yes  -- throw away formating WS
                               ] file

atTag tag = deep (isElem >>> hasName tag)

textAtTag str = atTag str >>> getChildren >>> getText

--Type für einen Arrow, der ein objekt vom typ a zurückgibt
type ParseArrow a = (IOSLA (XIOState ())) (Data.Tree.NTree.TypeDefs.NTree XNode) a

parseToList :: String -> String -> String -> ParseArrow a -> IO [a]
parseToList file mainNode subnode arrow = runX $ parseXML file >>> atTag mainNode >>> atTag subnode >>> arrow


parseZeiteinheit :: ParseArrow Zeiteinheit
parseZeiteinheit =  proc node -> do
  nid   <- textAtTag "id"    -< node
  titel <- textAtTag "Titel" -< node
  beschreibung <- withDefault (textAtTag "Beschreibung") "" -< node
  pe    <- withDefault (textAtTag "Physikeinheit") "Nein"   -< node
  exk   <- withDefault (textAtTag "Exkursion") "Nein"       -< node
  zeit  <- textAtTag "Zeit"  -< node
  ort   <- withDefault (textAtTag "Ort") "" -< node
  let typ | pe  == "Ja" = Physikeinheit
          | exk == "Ja" = Exkursionseinheit
          | otherwise   = Andere
  returnA -< Zeiteinheit { znode = Node (read nid) titel
                        , ztyp = typ
                        , zbeschreibung = beschreibung
                        , zort = ort
                        , zzeitspanne = stringZuZeitspanne zeit}

parseRaum :: ParseArrow Raum
parseRaum = proc node -> do
  nid    <- textAtTag "id"       -< node
  titel  <- textAtTag "Name"     -< node
  rgr    <- textAtTag "Raumgr-e" -< node
  returnA -< Raum { rnode = Node (read nid) titel
                  , rgrosse = read rgr}

parseThema :: ParseArrow Thema
parseThema = proc node -> do
  nid       <- textAtTag "id"    -< node
  titel     <- textAtTag "Thema" -< node
  typstring <- textAtTag "Typ"   -< node
  let typ  | typstring == "Vortrag"         = Vortrag
           | typstring == "Aufgabenseminar" = Aufgabenseminar
           | typstring == "Experiment"      = Experiment
           | typstring == "Andere"          = Exkursion
  maxTeilnehmerStr <- withDefault (textAtTag "MaximaleTeilnehmerzahl") "-1" -< node

  returnA -< Thema { tnode = Node (read nid) titel
                   , ttyp=typ
                   , tMaxTeilnehmer = teilnehmeranzahlZuInt maxTeilnehmerStr}

teilnehmeranzahlZuInt :: String -> Maybe Int
teilnehmeranzahlZuInt "-1" = Nothing
teilnehmeranzahlZuInt x = Just $ read x

parseThemenwahl :: ParseArrow ThemenwahlIndex
parseThemenwahl = proc node -> do
  themaId   <- textAtTag "Thema"    -< node
  userId    <- textAtTag "Benutzer" -< node
  wahl      <- textAtTag "Wahl"     -< node
  returnA -< (read themaId,read userId,read wahl)


parseTeilnehmer :: [ThemenwahlIndex]-> [Thema] -> ParseArrow User
parseTeilnehmer themenwahlenIndex themen = proc node -> do
  rolle    <- withDefault (textAtTag "Rollen") "Teilnehmer" -< node
  if rolle /= "Teilnehmer"
    then zeroArrow -< ()
    else do
      userid   <- textAtTag "id"  -< node
      name     <- textAtTag "Name"  -< node
      vorname  <- textAtTag "Vorname"  -< node
      nachname <- textAtTag "Nachname"  -< node
      let filteredThemenwahlen = filter (\(_,uid',_)-> (read userid)==uid') themenwahlenIndex
      let themenwahlen = map (\(nid,_,wahl)->(findeByNidError "Thema" themen nid, wahl)) filteredThemenwahlen
      returnA -< User { uid = read userid
                      , uvorname = vorname
                      , unachname = nachname
                      , uname = name
                      , uort = "Ort"
                      , uthemenwahlen = themenwahlen}

parseBetreuer :: [ThemenwahlIndex]-> [Thema] -> ParseArrow User
parseBetreuer themenwahlenIndex themen = proc node -> do
  rolle    <- withDefault (textAtTag "Rollen") "Teilnehmer" -< node
  if rolle == "Teilnehmer"
    then zeroArrow -< ()
    else do
      userid   <- textAtTag "id"  -< node
      name     <- textAtTag "Name"  -< node
      vorname  <- textAtTag "Vorname"  -< node
      nachname <- textAtTag "Nachname"  -< node
      let filteredThemenwahlen = filter (\(_,uid',_)-> (read userid)==uid') themenwahlenIndex
      let themenwahlen = map (\(nid,_,wahl)->(findeByNidError "Thema" themen nid, wahl)) filteredThemenwahlen
      returnA -< User { uid = read userid
                      , uvorname = vorname
                      , unachname = nachname
                      , uname = name
                      , uort = "Ort"
                      , uthemenwahlen = themenwahlen}


parsePlaneinheit :: ParseArrow (Int, String, String, String)
parsePlaneinheit = proc node -> do
  einheit <- textAtTag "einheit" -< node
  thema   <- textAtTag "thema" -< node
  betreuer <- textAtTag "betreuer" -< node
  raum <- textAtTag "raum" -< node
  returnA -< (read einheit, thema, betreuer, raum)


parseZuweisung :: [Thema] -> [User] -> ParseArrow (Thema, User)
parseZuweisung themen teilnehmer = proc node -> do
  themaStr <- textAtTag "thema" -< node
  teilnehmerStr <- textAtTag "teilnehmer" -< node
  returnA -< ( findeByNameError "Zuweisungen" themen themaStr, findeByNameError "zuweisung" teilnehmer teilnehmerStr)


parseVoraussetzung :: [Thema] -> ParseArrow (Thema, Thema)
parseVoraussetzung themen = proc node -> do
  voraussetzend <- textAtTag "voraussetzend" -< node
  voraussetzung <- textAtTag "voraussetzung" -< node
  let voraussetzendThema = findeByNid themen $ Nid (read voraussetzend)
  case voraussetzendThema of
    Nothing -> zeroArrow -< ()
    Just t -> returnA -< (t, findeByNidError "Voraussetzung" themen (Nid (read voraussetzung)))
