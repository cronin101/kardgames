module Models.Hands where

import           Data.List
import           Models.Cards

data Pair =
  Pair CardValue Suit Suit
  deriving (Eq, Show, Read)

instance Ord Pair where
  compare (Pair valueA _ _) (Pair valueB _ _) = compare valueA valueB

data ThreeOfAKind =
  ThreeOfAKind CardValue Suit Suit Suit
  deriving (Eq, Show, Read)

instance Ord ThreeOfAKind where
  compare (ThreeOfAKind valueA _ _ _) (ThreeOfAKind valueB _ _ _) =
    compare valueA valueB

data FourOfAKind =
  FourOfAKind CardValue Suit Suit Suit Suit
  deriving (Eq, Show, Read)

instance Ord FourOfAKind where
  compare (FourOfAKind valueA _ _ _ _) (FourOfAKind valueB _ _ _ _) =
    compare valueA valueB

data CountOfValue =
  CountOfValue
    { count        :: Integer
    , countedValue :: CardValue
    }
  deriving (Eq, Show, Read)

instance Ord CountOfValue where
  compare (CountOfValue countA _) (CountOfValue countB _) =
    compare countA countB

data Hand
  = HighCard
      { cards :: [Card]
      }
  | PairHand
      { pair  :: Pair
      , cards :: [Card]
      }
  | TwoPairHand
      { bestPair   :: Pair
      , secondPair :: Pair
      , cards      :: [Card]
      }
  | ThreeOfAKindHand
      { threeOfAKind :: ThreeOfAKind
      , cards        :: [Card]
      }
  | Straight
      { cards :: [Card]
      }
  | Flush
      { suit  :: Suit
      , cards :: [Card]
      }
  | FullHouse
      { threeOfAKind :: ThreeOfAKind
      , pair         :: Pair
      , cards        :: [Card]
      }
  | FourOfAKindHand
      { fourOfAKind :: FourOfAKind
      , cards       :: [Card]
      }
  | StraightFlush
      { suit  :: Suit
      , cards :: [Card]
      }
  deriving (Eq, Ord, Show, Read)

findHighestCountOfValue :: [Card] -> CountOfValue
findHighestCountOfValue cards
  | null cards = CountOfValue 0 (Face Ace)
  | otherwise =
    snd $
    foldl checkForRunIncrease (initialCountOfValue, initialCountOfValue) sorted
  where
    sorted = sort cards
    initialCountOfValue = CountOfValue 0 (value (head sorted))
    checkForRunIncrease (CountOfValue count countedValue, maxCountOfValue) nextCard
      | countedValue == value nextCard =
        (nextCountWhenMatching, max maxCountOfValue nextCountWhenMatching)
      | otherwise = (CountOfValue 1 (value nextCard), maxCountOfValue)
      where
        nextCountWhenMatching = CountOfValue (count + 1) countedValue
