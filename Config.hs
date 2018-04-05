module Config where

import Data.List.Split

data Config = Config
  { cMinTeilnehmer :: Int
   ,cMaxTeilnehmer :: Int
   ,cExperimenteFuellen :: Bool
   ,cNutzeZwangszuweisungen :: Bool
   ,cNutzeVoraussetzungen :: Bool
   ,cWebsiteDaten :: String
   ,cGlobalplanXml :: String
   ,cInfo :: String
   ,cZwangszuweisungen ::String
   ,cStundenplanOut :: String
   ,cGlobalplanTex :: String
   ,cLokalplanTex :: String
   ,cNamensschilderPrefix :: String
   ,cNamensschilderTex :: String
  }

readConfig :: IO Config
readConfig = do
  minTeilnehmer <- readIntFromConfig "Minimale_Teilnehmerzahl"
  maxTeilnehmer <- readIntFromConfig "Maximale_Teilnehmerzahl"
  experimenteFuellen <- readBooleanFromConfig "Experimente_fuellen"
  nutzeZwangszuweisungen <- readBooleanFromConfig "Nutze_Zwangszuweisungen"
  nutzeVoraussetzugen <- readBooleanFromConfig "Nutze_Voraussetzungen"
  websiteDaten <- readStringFromConfig "Website_Daten"
  globalplanXml <- readStringFromConfig "Globalplan_Xml"
  info <- readStringFromConfig "Info"
  zwangszuweisungen <- readStringFromConfig "Zwangszuweisungen"
  stundenplanOut <- readStringFromConfig "Stundenplan_Out"
  globalplanTex <- readStringFromConfig "Globalplan_Tex"
  lokalplanTex <- readStringFromConfig "Lokalplan_Tex"
  namensschilderPrefix <- readStringFromConfig "Namensschilder_Prefix"
  namensschilderTex <- readStringFromConfig "Namensschilder_Tex"
  return $ Config { cMinTeilnehmer = minTeilnehmer
                  , cMaxTeilnehmer = maxTeilnehmer
                  , cExperimenteFuellen = experimenteFuellen
                  , cNutzeZwangszuweisungen = nutzeZwangszuweisungen
                  , cNutzeVoraussetzungen = nutzeVoraussetzugen
                  , cWebsiteDaten = websiteDaten
                  , cGlobalplanXml = globalplanXml
                  , cInfo = info
                  , cZwangszuweisungen = zwangszuweisungen
                  , cStundenplanOut = stundenplanOut
                  , cGlobalplanTex = globalplanTex
                  , cLokalplanTex = lokalplanTex
                  , cNamensschilderPrefix = namensschilderPrefix
                  , cNamensschilderTex = namensschilderTex}

readStringFromConfig :: String ->IO String
readStringFromConfig val = do
  string <- readFile "./stundenplan.config"
  let split = lines string
  let filtered = filter (\l -> l/="" && (head l)/='#') split
  let splitOnEquals = map (splitOn "=") filtered
  let foundValues = filter (\x -> (removeSpaces $ head x) == val) splitOnEquals
  if length foundValues == 0
    then error $ "Feld "++val++" existiert nicht"
    else if length foundValues >=2
      then error $ "Feld "++val ++" existiert mehrfach"
      else if (length $ head foundValues) /= 2
        then error $ "Feld "++val++" hat invalide Struktur"
        else return $ last $ head foundValues

readBooleanFromConfig :: String -> IO Bool
readBooleanFromConfig val = do
  str <- readStringFromConfig val
  return $ ((removeSpaces str)=="ja") || ((removeSpaces str) == "yes")

readIntFromConfig :: String -> IO Int
readIntFromConfig val = do
  str <- readStringFromConfig val
  return $ read str

removeSpaces:: String -> String
removeSpaces = filter (\c -> c /= ' ')
