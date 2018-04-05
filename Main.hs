{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment
import Data.List
import Data.Ord
import System.IO
import GHC.Exts
import Data.LinearProgram.GLPK

import Seminar
import Parser
import LPProblem
import LatexWriter
import Config

main :: IO()
main = do
  args <- getArgs
  config <- readConfig
  case args of
    ("BetreuerBewertungen":rest) -> zeigeBetreuerBewertungen rest config
    ("Stundenplan":rest)         -> generiereStundenplan rest config
    ("TexGlobalplan":rest)       -> texGlobalplan rest config
    ("TexLokalplan":rest)        -> texLokalplan rest config
    ("TexNamensschilder":rest)   -> texNamensschilder rest config
    ("Test":rest)                -> test
    (arg:rest)                   -> putStrLn $ "Unbekannter Befehl"++arg
    _                            -> komplett [] config

test :: IO()
test = do
  putStrLn "Nothing"
  {-let thema = Thema (Node (Nid 1) "Testthema") Vortrag Nothing
  let user1 = User (Nid 1) "Test1" "User" "User1" "Ort" [(thema, 100)]
  let user2 = User (Nid 2) "Test2" "User" "User2" "Ort" [(thema, 100)]
  let user3 = User (Nid 3) "Test3" "User" "User3" "Ort" [(thema, 100)]
  let user4 = User (Nid 4) "Test4" "User" "User4" "Ort" [(thema, 100)]
  let zeiteinheit = Zeiteinheit (Node (Nid 1) "Zeiteinheit") Physikeinheit "Test" "Ort" (stringZuZeitspanne "2000-01-01 16:00")
  let betreuer = User (Nid 5) "Test" "Betreuer" "Testbetreuer" "Ort" [(thema, 100)]
  let raum = Raum (Node (Nid 1) "Testraum") 15
  let seminar = Seminar [zeiteinheit] [raum] [thema] [user1, user2, user3, user4] [betreuer]
  let planeinheit = Planeinheit (Nid 1) zeiteinheit thema betreuer raum
  let globalplan = Globalplan seminar [planeinheit]
  putStrLn $ show $ solveLP globalplan
  lpBerechnung <- glpSolveVars orpheusLPOptionen $ solveLP globalplan
  putStrLn ""
  putStrLn $ show lpBerechnung-}

komplett :: [String] -> Config -> IO()
komplett args config = do
  generiereStundenplan args config
  texGlobalplan args config
  texLokalplan args config
  texNamensschilder args config

texNamensschilder :: [String] -> Config -> IO()
texNamensschilder args config = do
  seminar <- leseSeminar config
  schreibeNamensschilder seminar config

texLokalplan :: [String] -> Config -> IO()
texLokalplan args config = do
  lokalplanStr <- readFile $ cStundenplanOut config
  info <- readFile $ cInfo config
  schreibeLokalplan info (read lokalplanStr) $ cLokalplanTex config

texGlobalplan :: [String] -> Config -> IO()
texGlobalplan args config = do
  case args of
    []           -> do
      lokalplanStr <- readFile $ cStundenplanOut config
      schreibeGlobalplan (read lokalplanStr) $ cGlobalplanTex config
    ["ohnePlan"] -> do
      seminar <- leseSeminar config
      globalplan <- leseGlobalplan seminar $ cGlobalplanXml config
      schreibeGlobalplan (Lokalplan globalplan []) $ cGlobalplanTex config


generiereStundenplan :: [String] -> Config -> IO()
generiereStundenplan args config = do
  seminar <- leseSeminar config
  globalplan <- leseGlobalplan seminar $ cGlobalplanXml config
  lpBerechnung <- glpSolveVars orpheusLPOptionen $ solveLP globalplan config
  case lpBerechnung of
    (retCode, Nothing)   -> putStrLn $ "Fehlgeschlagen: " ++ show retCode
    (_, Just (obj, lpResult)) -> do
      let lokalplan = readLPToLokalplan globalplan lpResult
      writeFile (cStundenplanOut config) $ show lokalplan

zeigeBetreuerBewertungen :: [String] -> Config -> IO()
zeigeBetreuerBewertungen args config = do
  seminar <- leseSeminar config
  let liste = concat $ map (\b -> map (\(thema, wahl) -> (b, thema, wahl)) $ uthemenwahlen b ) $ sbetreuer seminar
  zeigeBewertungenListe liste args

zeigeBewertungenListe :: [(User, Thema, Int)] -> [String] ->IO()
zeigeBewertungenListe liste args = do
  let ordnung = head args
      stringlist = case ordnung of
                     "t" -> map (\(u,t,w) -> (titel (tnode t), uname u, w)) liste
                     "b" -> map (\(u,t,w) -> (uname u, titel (tnode t), w)) liste
      filtered = if (tail args)==[]
                  then stringlist
                  else filter (\(x,y,w) -> elem x (tail args)) stringlist
      grouped = groupWith (\(x,y,w) -> x) filtered
      sorted = sortWith (\x -> first (head x)) $ map (reverse.(sortWith (\(x,y,w)->w))) grouped
  mapM_ printSingle sorted

printSingle :: [(String, String, Int)] -> IO()
printSingle list = do
  putStrLn $ "\n"++ (first $ head list)
  mapM_ (\(x,y,w) -> putStrLn $"    " ++ y ++ ": " ++(show w)) list

first :: (String, String, Int) -> String
first (s,_,_) = s
