module Models.Cards where

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Face
  = Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data CardValue
  = Rank Rank
  | Face Face
  deriving (Eq, Ord, Show, Read)

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
