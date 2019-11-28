module Models.Cards where

import           Data.Ord

data CardValue
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

succCard Ace = Two
succCard c   = succ c

data Suit
  = Heart
  | Diamond
  | Spade
  | Club
  deriving (Eq, Show, Read, Enum, Bounded)

instance Ord Suit where
  compare a b = EQ

data Card =
  Card
    { suit  :: Suit
    , value :: CardValue
    }
  deriving (Show, Read, Ord, Eq)
