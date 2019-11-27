module Models.Hands where

import           Data.List
import           Data.Ord
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
      { flushSuit :: Suit
      , cards     :: [Card]
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
      { flushSuit :: Suit
      , cards     :: [Card]
      }
  deriving (Eq, Ord, Show, Read)

findHighestCountOfValue :: [Card] -> CountOfValue
findHighestCountOfValue cards
  | null cards = CountOfValue 0 (Face Ace)
  | otherwise =
    snd $
    foldl checkForRunIncrease (initialCountOfValue, initialCountOfValue) sorted
  where
    sorted = sortOn Data.Ord.Down cards
    initialCountOfValue = CountOfValue 0 (value (head sorted))
    checkForRunIncrease (CountOfValue count countedValue, maxCountOfValue) nextCard
      | countedValue == value nextCard =
        (nextCountWhenMatching, max maxCountOfValue nextCountWhenMatching)
      | otherwise = (CountOfValue 1 (value nextCard), maxCountOfValue)
      where
        nextCountWhenMatching = CountOfValue (count + 1) countedValue

scoreHandByKind :: [Card] -> Hand
scoreHandByKind cards
  | highestCountOfValue <= CountOfValue 1 (Face Ace) = HighCard cards
  | highestCountOfValue >= CountOfValue 4 (Rank Two) =
    FourOfAKindHand (asFourOfAKind highestCountOfValue) cards
  | highestCountOfValue >= CountOfValue 3 (Rank Two) =
    scoreThreeOfAKindOrFullHouse highestCountOfValue cards
  | highestCountOfValue >= CountOfValue 2 (Rank Two) =
    scorePairOrTwoPair highestCountOfValue cards
  where
    scoreThreeOfAKindOrFullHouse highestCount@(CountOfValue _ countedValue) cards =
      case scoreHandByKind (filter ((countedValue /=) . value) cards) of
        PairHand pair cards ->
          FullHouse (asThreeOfAKind highestCount) pair cards
        _ -> ThreeOfAKindHand (asThreeOfAKind highestCount) cards
    scorePairOrTwoPair highestCount@(CountOfValue _ countedValue) cards =
      case scoreHandByKind (filter ((countedValue /=) . value) cards) of
        PairHand pair cards -> TwoPairHand (asPair highestCount) pair cards
        _                   -> PairHand (asPair highestCount) cards
    highestCountOfValue = findHighestCountOfValue cards
    asFourOfAKind (CountOfValue _ value) =
      FourOfAKind
        value
        (head matchingSuits)
        (matchingSuits !! 1)
        (matchingSuits !! 2)
        (matchingSuits !! 3)
    asThreeOfAKind (CountOfValue _ value) =
      ThreeOfAKind
        value
        (head matchingSuits)
        (matchingSuits !! 1)
        (matchingSuits !! 2)
    asPair (CountOfValue _ value) =
      Pair value (head matchingSuits) (matchingSuits !! 1)
    matchingSuitsForHighestCount (CountOfValue count countedValue) =
      map suit $ filter (\card -> value card == countedValue) cards
    matchingSuits = matchingSuitsForHighestCount highestCountOfValue
