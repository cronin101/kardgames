module Models.Hands where

import           Data.List
import           Data.Ord
import           Models.Cards

data Pair =
  Pair CardValue (Suit, Suit)
  deriving (Eq, Show, Read, Ord)

data ThreeOfAKind =
  ThreeOfAKind CardValue (Suit, Suit, Suit)
  deriving (Eq, Show, Read, Ord)

data CardsOfValue =
  CardsOfValue
    { count        :: Int
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

makeRuns :: [Card] -> [[Card]]
makeRuns cards = foldr growOrSplit [[]] (descendingCards ++ acesLow)
  where
    acesLow = filter ((Face Ace ==) . value) descendingCards
    descendingCards = sortOn Data.Ord.Down cards
    growOrSplit e r
      | null $ head r = [[e]]
      | endValue == startValue || endValue == succCard startValue =
        (e : head r) : tail r
      | otherwise = [e] : r
      where
        endValue = value e
        startValue = value $ head $ head r

makeStraights :: [Card] -> [Hand]
makeStraights cards =
  map makeStraightOrStraightFlush $ filter ((>= 5) . length) (makeRuns cards)
  where
    makeStraightOrStraightFlush run
      | length groupedByFlush == 5 =
        StraightFlush (suit $ head groupedByFlush) (value $ head groupedByFlush)
      | otherwise = Straight (value $ head run)
      where
        groupedByFlush = sortOn Data.Ord.Down $ head (groupBy equalSuit run)
        equalSuit x y = suit x == suit y

descendingStrengthCardsOfValue :: [Card] -> [CardsOfValue]
descendingStrengthCardsOfValue =
  map toCardsOfValue . group . sortOn Data.Ord.Down
  where
    toCardsOfValue cs = length cs `CardsOfValue` value (head cs)

scoreHandForSameRank :: [Card] -> Hand
scoreHandForSameRank cards =
  case descendingStrengthCardsOfValue cards of
    4 `CardsOfValue` v:_ -> FourOfAKind v
    3 `CardsOfValue` v3:2 `CardsOfValue` v2:_ ->
      FullHouse
        (ThreeOfAKind v3 (arrayToT3 $ suitsFor v3))
        (Pair v2 (arrayToT2 $ suitsFor v2))
    3 `CardsOfValue` v:_ ->
      ThreeOfAKindH $ ThreeOfAKind v (arrayToT3 $ suitsFor v)
    2 `CardsOfValue` v1:2 `CardsOfValue` v2:_ ->
      TwoPairH
        (Pair v1 (arrayToT2 $ suitsFor v1))
        (Pair v2 (arrayToT2 $ suitsFor v2))
    2 `CardsOfValue` v:_ -> PairH $ Pair v (arrayToT2 $ suitsFor v)
    1 `CardsOfValue` v:_ -> HighCard v
  where
    suitsFor v = map suit $ filter ((== v) . value) cards
    arrayToT2 a = (head a, a !! 1)
    arrayToT3 a = (head a, a !! 1, a !! 2)
