module Seminar where

import Data.Maybe
import Data.Time
import Data.List.Split
import Control.Arrow (first)

--ID aller objekte
newtype Nid = Nid Integer
    deriving (Ord, Eq)

--das versteh ich noch nicht wirklich
instance Read Nid where
  readsPrec prec = map (first Nid) . readsPrec prec

instance Show Nid where
  show (Nid id) = show id

--Node, d.h. id und name für alle objekte
data Node = Node
  { nid   :: Nid
  , titel :: String
  }
  deriving (Show, Eq, Ord, Read)

unique :: [a] -> Maybe a
unique [a] = Just a
unique _   = Nothing

class ContainsNid a where
  theNid :: a -> Nid

  findeByNid :: [a] -> Nid -> Maybe a
  findeByNid as nid = unique $ filter (matchNid nid) as

  findeByNidError :: String -> [a] -> Nid -> a
  findeByNidError msg as nid = fromMaybe
    (error $ msg ++ " (" ++ show nid ++ ") nicht gefunden")
    (findeByNid as nid)

  matchNid :: Nid -> a -> Bool
  matchNid nid a = nid == theNid a

class ContainsName a where
  theName :: a -> String

  findeByName :: [a] -> String -> Maybe a
  findeByName as name = unique $ filter (matchName name) as

  findeByNameError :: String -> [a] -> String -> a
  findeByNameError msg as name = fromMaybe
    (error $ msg ++ ": "++name++" nicht gefunden")
    (findeByName as name)

  matchName :: String -> a -> Bool
  matchName name a = name == theName a

data ThemaTyp = Vortrag | Aufgabenseminar | Experiment | Exkursion
  deriving (Show, Read, Eq)

data Thema = Thema
  { tnode :: Node
  , ttyp  :: ThemaTyp
  , tMaxTeilnehmer :: Maybe Int
  }
  deriving (Show, Read, Eq)

instance ContainsNid Thema where
  theNid = nid.tnode

instance ContainsName Thema where
  theName = titel.tnode

data Zeitspanne = Zeitspanne
  { datum :: Day
  , start :: TimeOfDay
  , ende  :: Maybe TimeOfDay
  }
  deriving (Show, Read, Eq)

instance Ord Zeitspanne where
  compare (Zeitspanne d1 s1 e1) (Zeitspanne d2 s2 e2) =
    let datumVergleich = compare d1 d2
    in if datumVergleich == EQ
      then compare s1 s2
      else datumVergleich

--parst einen String der Form "yyyy-mm-dd hh:mm (bis hh:mm)" zu einer Zeitspanne
stringZuZeitspanne :: String -> Zeitspanne
stringZuZeitspanne str =
  let splitSpace = splitOn " " str
      splitDate = splitOn "-" (splitSpace !! 0)
      splitStart = splitOn ":" (splitSpace !! 1)
  in if (length splitSpace) == 2
      then Zeitspanne {
              datum = fromGregorian (read $ splitDate !! 0) (read $ splitDate !! 1) (read $ splitDate !! 2)
            , start = TimeOfDay (read $ splitStart !! 0) (read $ splitStart !! 1) 0
            , ende = Nothing
              }
      else
        let splitEnde = splitOn ":" (splitSpace !! 3)
        in  Zeitspanne {
                 datum = fromGregorian (read $ splitDate !! 0) (read $ splitDate !! 1) (read $ splitDate !! 2)
               , start = TimeOfDay (read $ splitStart !! 0) (read $ splitStart !! 1) 0
               , ende  = Just $ TimeOfDay (read $ splitEnde !! 0) (read $ splitEnde !! 1) 0
                 }


data ZeiteinheitType = Physikeinheit | Exkursionseinheit | Andere
  deriving (Show, Read, Eq)

data Zeiteinheit = Zeiteinheit
  { znode          :: Node
  , ztyp           :: ZeiteinheitType
  , zbeschreibung  :: String
  , zort           :: String
  , zzeitspanne    :: Zeitspanne
  }
  deriving (Show, Read, Eq)

--Ob diese Einheit eine Physikeinheit oder eine Exkursion ist
istPEOderExk :: Zeiteinheit -> Bool
istPEOderExk z = (ztyp z) == Physikeinheit || (ztyp z) == Exkursionseinheit

instance ContainsNid Zeiteinheit where
  theNid = nid.znode

instance ContainsName Zeiteinheit where
  theName = titel.znode

data Raum = Raum
  { rnode :: Node
  , rgrosse  :: Int
  }
  deriving (Show, Read, Eq)

instance ContainsNid Raum where
  theNid = nid.rnode

instance ContainsName Raum where
  theName = titel.rnode

--Sowohl teilnehmer als auch betreuer
data User = User
  { uid            :: Nid
  , uvorname       :: String
  , unachname      :: String
  , uname          :: String
  , uort           :: String
  , uthemenwahlen  :: [(Thema, Int)]
  }
  deriving (Show, Read, Eq)

findeThemenwahl :: User -> Thema -> Double
findeThemenwahl user thema = head $ [fromIntegral val | (t, val) <- uthemenwahlen user, t == thema] ++ [0.0]

instance ContainsNid User where
  theNid = uid

instance ContainsName User where
  theName user = (uvorname user) ++ " " ++ (unachname user)

data Seminar = Seminar
  { szeiteinheiten :: [Zeiteinheit]
  , sraueme        :: [Raum]
  , sthemen        :: [Thema]
  , steilnehmer    :: [User]
  , sbetreuer      :: [User]
  , szwangszuweisungen :: [(Thema, User)]
  , svoraussetzungen :: [(Thema, Thema)]  -- (Voraussetzend, Voraussetzung)
  }
  deriving (Show, Read, Eq)

data Planeinheit = Planeinheit
  { pid           :: Nid
  , pZeiteinheit  :: Zeiteinheit
  , pThema        :: Thema
  , pBetreuer     :: User
  , pRaum         :: Raum
  }
  deriving (Show, Read, Eq)

data Globalplan = Globalplan
  { gSeminar   :: Seminar
  , gEinheiten :: [Planeinheit]
  }
  deriving (Show, Read, Eq)

data Lokalplan = Lokalplan
  { lGlobalplan :: Globalplan
  , lBelegungen :: [(User, Planeinheit)]
  }
  deriving (Show, Read, Eq)
