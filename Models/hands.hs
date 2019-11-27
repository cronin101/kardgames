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

maxCountOfAnyRank :: [Card] -> Integer
maxCountOfAnyRank cards
  | null cards = 0
  | otherwise =
    snd $ foldl checkForRunIncrease (0, 0, value $ head sorted) sorted
  where
    sorted = sort cards
    checkForRunIncrease (currentCount, maxCount, currentValue) nextCard
      | currentValue == value nextCard =
        (currentCount + 1, max maxCount (currentCount + 1), currentValue)
      | otherwise = (1, maxCount, value nextCard)
    snd (_, maxCount, _) = maxCount
