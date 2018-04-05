{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module LatexWriter where

import GHC.Exts
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.List.Split

import Seminar
import Config

schreibeGlobalplan :: Lokalplan -> String -> IO()
schreibeGlobalplan lokalplan file =
  execLaTeXT (addPreamble (globalplanZuTex lokalplan)) >>= renderFile file

addPreamble :: LaTeXT_ IO -> LaTeXT_ IO
addPreamble content=do
      thePreamble
      document content


-- Preamble with some basic info.
thePreamble :: LaTeXT_ IO
thePreamble = do
    documentclass [a4paper] article
    usepackage [] "fullpage"
    usepackage ["german", "ngerman"] "babel"
    usepackage ["utf8"] "inputenc"
    usepackage [] "graphicx"
    raw $ fromString "\\pagenumbering{gobble}"

globalplanZuTex :: Lokalplan -> LaTeXT_ IO
globalplanZuTex lokalplan = do
  let einheitenGruppiert = groupWith (zzeitspanne.pZeiteinheit) $ gEinheiten $ lGlobalplan lokalplan
  mconcat $ [raw "~" , lnbk] ++ (map (zeiteinheitGlobalZuTex lokalplan) einheitenGruppiert)

zeiteinheitGlobalZuTex :: Lokalplan -> [Planeinheit] -> LaTeXT_ IO
zeiteinheitGlobalZuTex lokalplan einheiten = do
  let content = mconcat $ map (planeinheitGlobalZuTex lokalplan) einheiten
  mconcat [   textbf $ large $ fromString $ zeitspanneZuString $ zzeitspanne $ pZeiteinheit $ head einheiten
            , raw "\\nopagebreak "
            , lnbk
            , raw "\\begin{tabular} {|p{5cm}|p{4cm}|p{4cm}|p{2cm}|}"
            , hline
            , ((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum")& (textbf "Anzahl"))
            , lnbk
            , hline
            , content
            , raw "\\end{tabular}"
            , lnbk
            , vspace (Mm 5)
            , raw "~"
            , lnbk
            ]

planeinheitGlobalZuTex ::Lokalplan -> Planeinheit -> LaTeXT_ IO
planeinheitGlobalZuTex lokalplan einheit = do
  let thema = theName $ pThema einheit
  let betreuer = theName $ pBetreuer einheit
  let raum = theName $ pRaum einheit
  let anzahl = show $ teilnehmerzahl lokalplan einheit
  mconcat [ (fromString thema) & (fromString betreuer) & (fromString raum) & (fromString anzahl)
          , lnbk
          , hline ]


schreibeLokalplan ::String -> Lokalplan -> String -> IO()
schreibeLokalplan info lokalplan file =
  execLaTeXT (addPreamble (lokalplanZuTex info lokalplan)) >>= renderFile file

lokalplanZuTex ::String -> Lokalplan -> LaTeXT_ IO
lokalplanZuTex info lokalplan =
  let teilnehmerSortiert = sortWith uvorname $ steilnehmer $ gSeminar $ lGlobalplan lokalplan
      betreuerSortiert = sortWith uvorname $ sbetreuer $ gSeminar $ lGlobalplan lokalplan
  in mconcat $ (map (schreibeTeilnehmerPlan info lokalplan) teilnehmerSortiert) ++ (map (schreibeBetreuerPlan info lokalplan) betreuerSortiert)

schreibeTeilnehmerPlan :: String -> Lokalplan -> User -> LaTeXT_ IO
schreibeTeilnehmerPlan info lokalplan teilnehmer =
  let tage = groupWith (datum.zzeitspanne) $ szeiteinheiten $ gSeminar $ lGlobalplan lokalplan
  in mconcat [ center $ huge $ textbf $ fromString $ "Stundenplan von " ++ (theName teilnehmer)
             , mconcat $ map (schreibeTagTeilnehmer lokalplan teilnehmer) tage
             , raw $ fromString info
             , newpage
             ]

schreibeBetreuerPlan :: String -> Lokalplan -> User -> LaTeXT_ IO
schreibeBetreuerPlan info lokalplan betreuer =
  let tage = groupWith (datum.zzeitspanne) $ szeiteinheiten $ gSeminar $ lGlobalplan lokalplan
  in mconcat [ center $ huge $ textbf $ fromString $ "Stundenplan von " ++ (theName betreuer)
             , mconcat $ map (schreibeTagBetreuer lokalplan betreuer) tage
             , raw $ fromString info
             , newpage
             ]

schreibeTagTeilnehmer :: Lokalplan -> User -> [Zeiteinheit] -> LaTeXT_ IO
schreibeTagTeilnehmer lokalplan teilnehmer einheiten =
  mconcat [ textbf $ large $ fromString $ datumZuTag $ datum $ zzeitspanne $ head einheiten
          , raw "\\nopagebreak "
          , lnbk
          , raw "\\begin{tabular} {|p{3cm} p{6cm} p{6cm}| }"
          ,hline
          , mconcat $ map (schreibeZeiteinheitTeilnehmer lokalplan teilnehmer) einheiten
          ,raw "\\end{tabular}"
          ,lnbk
          , vspace (Mm 5)
          , raw "~"
          , lnbk
          ]

schreibeTagBetreuer :: Lokalplan -> User -> [Zeiteinheit] -> LaTeXT_ IO
schreibeTagBetreuer lokalplan betreuer einheiten =
  mconcat [ textbf $ large $ fromString $ datumZuTag $ datum $ zzeitspanne $ head einheiten
          , raw "\\nopagebreak "
          , lnbk
          , raw "\\begin{tabular} {|p{3cm} p{6cm} p{6cm}| }"
          ,hline
          , mconcat $ map (schreibeZeiteinheitBetreuer lokalplan betreuer) einheiten
          ,raw "\\end{tabular}"
          ,lnbk
          , vspace (Mm 5)
          , raw "~"
          , lnbk
          ]

schreibeZeiteinheitTeilnehmer :: Lokalplan -> User -> Zeiteinheit -> LaTeXT_ IO
schreibeZeiteinheitTeilnehmer lokalplan teilnehmer zeiteinheit =
  let zeit = zeitspanneZuStringOhneTag $ zzeitspanne zeiteinheit
  in if istPEOderExk zeiteinheit
      then
        let planeinheiten = [p | (t,p) <- lBelegungen lokalplan, t==teilnehmer, pZeiteinheit p == zeiteinheit]
        in if length planeinheiten == 0
          then mconcat [ (textbf.fromString) zeit & fromString "frei" & fromString ""
                       , lnbk
                       , hline ]
          else
            let thema = theName $ pThema $ head planeinheiten
                raum = theName $ pRaum $ head planeinheiten
                betreuer = theName $ pBetreuer $ head planeinheiten
                anzahl = show $ teilnehmerzahl lokalplan $ head planeinheiten
            in mconcat [ (textbf.fromString) zeit & (textbf.fromString) thema & (textbf.fromString) raum
                       , lnbk
                       , "" & fromString ("Betreuer: " ++ betreuer) & fromString ("ca. " ++ anzahl ++ " Teilnehmer")
                       , lnbk
                       , hline]
      else
        let name = theName zeiteinheit
            ort = zort zeiteinheit
        in if zbeschreibung zeiteinheit == ""
            then
              mconcat [ (textbf.fromString) zeit & (textbf.fromString) name & (textbf.fromString) ort
                         , lnbk
                         , hline]
            else
              mconcat [ (textbf.fromString) zeit & (textbf.fromString) name & (textbf.fromString) ort
                         , lnbk
                         , "" & (fromString.cleanBeschreibung.zbeschreibung) zeiteinheit & ""
                         , lnbk
                         , hline]

schreibeZeiteinheitBetreuer :: Lokalplan -> User -> Zeiteinheit -> LaTeXT_ IO
schreibeZeiteinheitBetreuer lokalplan betreuer zeiteinheit =
  let zeit = zeitspanneZuStringOhneTag $ zzeitspanne zeiteinheit
  in if istPEOderExk zeiteinheit
      then
        let planeinheiten = filter (\p -> ((pBetreuer p)==betreuer) && ((pZeiteinheit p)==zeiteinheit)) $ gEinheiten $ lGlobalplan lokalplan
        in if length planeinheiten == 0
          then mconcat [ (textbf.fromString) zeit & fromString "frei" & fromString ""
                       , lnbk
                       , hline ]
          else
            let thema = theName $ pThema $ head planeinheiten
                raum = theName $ pRaum $ head planeinheiten
                anzahl = show $ teilnehmerzahl lokalplan $ head planeinheiten
            in mconcat [ (textbf.fromString) zeit & (textbf.fromString) thema & (textbf.fromString) raum
                       , lnbk
                       , ""  & fromString ("ca. " ++ anzahl ++ " Teilnehmer") & ""
                       , lnbk
                       , hline]
      else
        let name = theName zeiteinheit
            ort = zort zeiteinheit
        in if zbeschreibung zeiteinheit == ""
            then
              mconcat [ (textbf.fromString) zeit & (textbf.fromString) name & (textbf.fromString) ort
                         , lnbk
                         , hline]
            else
              mconcat [ (textbf.fromString) zeit & (textbf.fromString) name & (textbf.fromString) ort
                         , lnbk
                         , "" & (fromString.cleanBeschreibung.zbeschreibung) zeiteinheit & ""
                         , lnbk
                         , hline]

cleanBeschreibung :: String -> String
cleanBeschreibung "" = ""
cleanBeschreibung  beschreibung = take ((length beschreibung) -8) $ drop 3 beschreibung

zeitspanneZuStringOhneTag :: Zeitspanne -> String
zeitspanneZuStringOhneTag (Zeitspanne datum start (Just ende)) =
  (zeitZuString start) ++ " bis " ++ (zeitZuString ende)
zeitspanneZuStringOhneTag (Zeitspanne datum start Nothing) =
  (zeitZuString start)

zeitspanneZuString :: Zeitspanne -> String
zeitspanneZuString (Zeitspanne datum start (Just ende)) =
  (datumZuTag datum) ++ " " ++ (zeitZuString start) ++ " bis " ++ (zeitZuString ende)
zeitspanneZuString (Zeitspanne datum start Nothing) =
    (datumZuTag datum) ++ " " ++ (zeitZuString start)

datumZuTag :: Day -> String
datumZuTag tag =
   let (_,_,wtag) = toWeekDate tag
   in ["Montag","Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"] !!  (wtag-1)

zeitZuString :: TimeOfDay -> String
zeitZuString zeit =
   let s = splitOn ":" $ show zeit
   in (s !! 0) ++ ":" ++ (s !! 1)

teilnehmerzahl :: Lokalplan -> Planeinheit -> Int
teilnehmerzahl lokalplan einheit =
  length $ filter (\(t,e) -> e == einheit) $ lBelegungen lokalplan



positionen :: [(Double, Double)]
positionen =[(-4.5,11.2),(-4.5,5.6),(-4.5,0.0),(-4.5,-5.6),(-4.5,-11.2),(4.5,11.2),(4.5,5.6),(4.5,0.0),(4.5,-5.6),(4.5,-11.2)]

side :: String
side ="\\begin{tikzpicture}[remember picture, overlay, decoration={random steps, segment length=1mm, amplitude=.5mm}]\n"

schreibeNamensschilder :: Seminar -> Config -> IO ()
schreibeNamensschilder seminar config = do
  prefix <- readFile $ cNamensschilderPrefix config
  writeFile (cNamensschilderTex config) $ schreibeNamensschilderZuString seminar prefix


schreibeNamensschilderZuString :: Seminar -> String -> String
schreibeNamensschilderZuString seminar prefix =
  let personenAufSeite = sortierePersonen seminar
  in prefix ++ (concat (map schreibeSeite personenAufSeite)) ++"\n \\end{document}"

sortierePersonen :: Seminar -> [[User]]
sortierePersonen seminar = chunksOf 10 $ sortWith uvorname $ steilnehmer seminar

schreibeSeite :: [User] -> String
schreibeSeite personen =
  side ++
  (vorderseite 0 personen) ++
  "\\end{tikzpicture}\\newpage\n" ++
  side ++
  (rueckseite 0) ++
  "\\end{tikzpicture}\\newpage\n"

vorderseite ::Int -> [User] -> String
vorderseite  10 _ =""
vorderseite _ [] = ""
vorderseite n (p:ps) =(vorderseite' (positionen !! n) p)++(vorderseite (n+1) ps)

vorderseite' :: (Double,Double) -> User -> String
vorderseite' (x,y) p =
  "\\Namensschild{"++
  (show x)++
  "cm}{" ++
  (show y)++
  "cm}{"++
  (uvorname p) ++" "++(unachname p)++ "}{ "++ (uort p) ++"}\n"

rueckseite :: Int -> String
rueckseite 10 =""
rueckseite n = (rueckseite' (positionen !! n)) ++ (rueckseite (n+1))

rueckseite' :: (Double,Double) -> String
rueckseite' (x,y)=
  "\\Rueckseite{"++
  (show x) ++
  "cm}{"++
  (show y) ++
  "cm}\n"
