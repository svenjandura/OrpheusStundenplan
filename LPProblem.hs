module LPProblem where

import Data.LinearProgram hiding ((+),(-),(*),(/))
import Data.LinearProgram.GLPK.Solver
import Data.Map
import Control.Monad

import Seminar
import Config

orpheusLPOptionen :: GLPOpts
orpheusLPOptionen = mipDefaults
  { tmLim = 1000 -- Nach 1000 Sekunden
  , mipGap = 0.1 -- 10 % Abstand zum Optimum sind ausreichend
  }

type LPSeminarFunk = Globalplan -> Config -> LPM String Double ()

solveLP :: Globalplan -> Config -> LP String Double
solveLP globalplan config = execLPM $ do
  optimum globalplan config
  bedingungen globalplan config
  setzeVariablentypen globalplan config

setzeVariablentypen :: LPSeminarFunk
setzeVariablentypen globalplan config = sequence_ $ do
  teilnehmer <- steilnehmer $ gSeminar globalplan
  planeinheit <- gEinheiten globalplan
  return $ setVarKind (var teilnehmer planeinheit) BinVar

freieEinheitenSummand :: Config -> Double
freieEinheitenSummand config = if cVermeideFreieEinheiten config then 1 else 0

optimum :: LPSeminarFunk
optimum globalplan config = do
  setDirection Max
  setObjective $ linCombination [ ( (wahl teilnehmer planeinheit)+(freieEinheitenSummand config)
                                  , var teilnehmer planeinheit)
                                  | teilnehmer <- steilnehmer $ gSeminar globalplan
                                  ,  planeinheit <- gEinheiten globalplan
                                ]

bedingungen :: LPSeminarFunk
bedingungen globalplan config = do
  proZeiteinheitNurEinThema globalplan config
  jedesThemaNurEinmal globalplan config
  maxTeilnehmerzahl globalplan config
  minTeilnehmerzahl globalplan config
  when (cExperimenteFuellen config) $ experimenteFuellen globalplan config
  when (cNutzeZwangszuweisungen config) $ zwangszuweisungenEinhalten globalplan config

proZeiteinheitNurEinThema :: LPSeminarFunk
proZeiteinheitNurEinThema globalplan config =
  sequence_ $ do
    zeiteinheit <- szeiteinheiten $ gSeminar globalplan
    teilnehmer  <- steilnehmer $ gSeminar globalplan
    return $ add [linCombination [(1, var teilnehmer p)] | p <- gEinheiten globalplan, pZeiteinheit p == zeiteinheit] `leqTo` 1

jedesThemaNurEinmal :: LPSeminarFunk
jedesThemaNurEinmal globalplan config = do
  sequence_ $ do
    teilnehmer <- steilnehmer $ gSeminar globalplan
    thema <- sthemen $ gSeminar globalplan
    return $ add [linCombination [(1, var teilnehmer p)] | p <- gEinheiten globalplan, pThema p == thema] `leqTo` 1

maxTeilnehmerzahl :: LPSeminarFunk
maxTeilnehmerzahl globalplan config = do
  sequence_ $ do
    planeinheit <- gEinheiten globalplan
    return $ add [linCombination [(1,var t planeinheit)] | t <- steilnehmer $ gSeminar globalplan] `leqTo` (fromIntegral $ maxTeilnehmerzahlVonPlaneinheit planeinheit config)

experimenteFuellen :: LPSeminarFunk
experimenteFuellen globalplan config = do
  sequence_ $ do
    planeinheit <- Prelude.filter (\p -> (ttyp $ pThema p)==Experiment) $ gEinheiten globalplan
    return $ add [linCombination [(1,var t planeinheit)] | t <- steilnehmer $ gSeminar globalplan] `equalTo` (fromIntegral $ maxTeilnehmerzahlVonPlaneinheit planeinheit config)

maxTeilnehmerzahlVonPlaneinheit :: Planeinheit -> Config -> Int
maxTeilnehmerzahlVonPlaneinheit planeinheit config =
  case tMaxTeilnehmer $pThema $ planeinheit of
    Nothing -> minimum [cMaxTeilnehmer config, rgrosse $ pRaum planeinheit]
    Just t ->  minimum [cMaxTeilnehmer config, rgrosse $ pRaum planeinheit,t]

minTeilnehmerzahl :: LPSeminarFunk
minTeilnehmerzahl globalplan config = do
  sequence_ $ do
    planeinheit <- gEinheiten globalplan
    return $ add [linCombination [(1,var t planeinheit)] | t <- steilnehmer $ gSeminar globalplan] `geqTo` (fromIntegral $ minTeilnehmerzahlVonPlaneinheit planeinheit config)

minTeilnehmerzahlVonPlaneinheit :: Planeinheit -> Config -> Int
minTeilnehmerzahlVonPlaneinheit planeinheit config = cMinTeilnehmer config

zwangszuweisungenEinhalten :: LPSeminarFunk
zwangszuweisungenEinhalten globalplan config = do
  sequence_ $ do
    (thema, teilnehmer) <- szwangszuweisungen $ gSeminar globalplan
    return $ add [linCombination [(1, var teilnehmer p)] | p <- gEinheiten globalplan, (pThema p) == thema] `equalTo` 1

unwissendeTeilnehmer :: Seminar -> Thema -> [User]
unwissendeTeilnehmer seminar thema = Prelude.filter (\u -> (findeThemenwahl u thema)/=0.0) $ steilnehmer seminar

readLPToLokalplan :: Globalplan -> Map String Double -> Lokalplan
readLPToLokalplan globalplan lpResult =
  let belegungen = [ (teilnehmer, planeinheit)
                      | teilnehmer <- steilnehmer $ gSeminar globalplan
                      , planeinheit <- gEinheiten globalplan
                      , (lpResult ! (var teilnehmer planeinheit))==1.0]
  in Lokalplan globalplan belegungen

wahl :: User -> Planeinheit -> Double
wahl user einheit = head $ [fromIntegral w | (thema, w) <- uthemenwahlen user, thema == pThema einheit] ++ [0]

var :: User -> Planeinheit -> String
var u p = (show (uid u)) ++ "_" ++ (show (pid p))
