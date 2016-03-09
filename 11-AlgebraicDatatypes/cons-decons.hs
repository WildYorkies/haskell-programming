-- cons-decons.hs
module Consdecons where
import Data.List (nub)

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b = 
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = 
  Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

-- Exercise!

data OperatingSystem = 
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill , Mac
  , Windows
  ]

data ProgrammingLanguage = 
  Haskell
  | Agda 
  | Idris
  | PureScript
  deriving (Eq, Show)

allLanguages :: [ProgrammingLanguage] 
allLanguages = [Haskell, Agda, Idris, PureScript]

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allProgrammers :: [Programmer] 
allProgrammers = nub [ Programmer {lang = i, os = j} | i <- allLanguages, j <- allOperatingSystems]




nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }




























