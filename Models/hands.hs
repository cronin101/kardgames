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
