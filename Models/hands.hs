module Models.Hands where

import           Data.List
import           Data.Ord
import           Models.Cards

data Pair =
  Pair CardValue (Suit, Suit)
  deriving (Eq, Show, Read)

instance Ord Pair where
  compare (Pair valueA _) (Pair valueB _) = compare valueA valueB

data ThreeOfAKind =
  ThreeOfAKind CardValue (Suit, Suit, Suit)
  deriving (Eq, Show, Read)

instance Ord ThreeOfAKind where
  compare (ThreeOfAKind valueA _) (ThreeOfAKind valueB _) =
    compare valueA valueB

data CardsOfValue =
  CardsOfValue
    { count        :: Integer
    , countedValue :: CardValue
    }
  deriving (Eq, Show, Read, Ord)

data Hand
  = HighCard
      { highCard :: CardValue
      }
  | PairH
      { pair :: Pair
      }
  | TwoPairH
      { bestPair   :: Pair
      , secondPair :: Pair
      }
  | ThreeOfAKindH
      { threeOfAKind :: ThreeOfAKind
      }
  | Straight
      { highCard :: CardValue
      }
  | Flush
      { flushSuit :: Suit
      , highCard  :: CardValue
      }
  | FullHouse
      { threeOfAKind :: ThreeOfAKind
      , pair         :: Pair
      }
  | FourOfAKind CardValue
  | StraightFlush
      { flushSuit :: Suit
      , highCard  :: CardValue
      }
  deriving (Eq, Ord, Show, Read)

findHighestCountOfSameRank :: [Card] -> CardsOfValue
findHighestCountOfSameRank cards
  | null cards = 0 `CardsOfValue` Face Ace
  | otherwise =
    snd $
    foldl checkForRunIncrease (initialCountOfValue, initialCountOfValue) sorted
  where
    sorted = sortOn Data.Ord.Down cards
    initialCountOfValue = 0 `CardsOfValue` value (head sorted)
    checkForRunIncrease (c `CardsOfValue` v, maxCountOfValue) nextCard
      | v == value nextCard =
        (nextCountWhenMatching, max maxCountOfValue nextCountWhenMatching)
      | otherwise = (1 `CardsOfValue` value nextCard, maxCountOfValue)
      where
        nextCountWhenMatching = (c + 1) `CardsOfValue` v

getHighCard :: [Card] -> Card
getHighCard = head . sortOn Data.Ord.Down

scoreHandForSameRank :: [Card] -> Hand
scoreHandForSameRank cards
  | highestCountOfSameRank <= 1 `CardsOfValue` Face Ace =
    HighCard $ value $ getHighCard cards
  | highestCountOfSameRank >= 4 `CardsOfValue` Rank Two =
    FourOfAKind (countedValue highestCountOfSameRank)
  | highestCountOfSameRank >= 3 `CardsOfValue` Rank Two =
    scoreThreeOfAKindOrFullHouse highestCountOfSameRank
  | highestCountOfSameRank >= 2 `CardsOfValue` Rank Two =
    scorePairOrTwoPair highestCountOfSameRank
  where
    scoreThreeOfAKindOrFullHouse highestCount@(_ `CardsOfValue` v) =
      case scoreHandForSameRank (cardsWithoutValue v) of
        PairH pair -> FullHouse (toThreeOfAKind highestCount) pair
        _          -> ThreeOfAKindH $ toThreeOfAKind highestCount
    scorePairOrTwoPair highestCount@(_ `CardsOfValue` v) =
      case scoreHandForSameRank (cardsWithoutValue v) of
        PairH pair -> TwoPairH (toPair highestCount) pair
        _          -> PairH $ toPair highestCount
    cardsWithoutValue v = filter ((v /=) . value) cards
    highestCountOfSameRank = findHighestCountOfSameRank cards
    matchingSuitsFor (count `CardsOfValue` v) =
      map suit $ filter (\card -> value card == v) cards
    matchingSuits = matchingSuitsFor highestCountOfSameRank
    toPair (_ `CardsOfValue` v) = Pair v suits2
    toThreeOfAKind (_ `CardsOfValue` v) = ThreeOfAKind v suits3
    suits2 = (head matchingSuits, matchingSuits !! 1)
    suits3 = (head matchingSuits, matchingSuits !! 1, matchingSuits !! 2)
