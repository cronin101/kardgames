module Models.Hands where

import           Data.Function
import           Data.List
import           Data.Ord
import           Models.Cards

newtype Pair =
  Pair CardValue
  deriving (Eq, Show, Read, Ord)

newtype ThreeOfAKind =
  ThreeOfAKind CardValue
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

makeRunsOfFive :: [Card] -> [[Card]]
makeRunsOfFive cards =
  filter ((== 5) . length) $
  concatMap (map (take 5) . tails) $
  foldr growOrSplit [[]] (descendingCards ++ acesLow)
  where
    acesLow = filter ((Ace ==) . value) descendingCards
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
makeStraights cards = map matchRun $ makeRunsOfFive cards
  where
    matchRun run =
      case groupedByFlush of
        [highCard, _, _, _, _] -> StraightFlush (suit highCard) (value highCard)
        _ -> Straight (value $ head run)
      where
        groupedByFlush = head (groupBy equalSuit run)
        equalSuit x y = suit x == suit y

makeFlushes :: [Card] -> [Hand]
makeFlushes cards =
  map (toFlush . head . sortOn Data.Ord.Down) $
  filter ((>= 5) . length) $
  map (\s -> filter (\card -> suit card == s) cards) suits
  where
    toFlush card = Flush (suit card) (value card)
    suits = enumFrom $ toEnum 0 :: [Suit]

descendingStrengthCardsOfValue :: [Card] -> [CardsOfValue]
descendingStrengthCardsOfValue =
  sortOn Data.Ord.Down . map toCardsOfValue . group . sort . map value
  where
    toCardsOfValue vs@(v:_) = length vs `CardsOfValue` v

scoreHandForSameRank :: [Card] -> Hand
scoreHandForSameRank cards =
  case descendingStrengthCardsOfValue cards of
    4 `CardsOfValue` v:_ -> FourOfAKind v
    3 `CardsOfValue` v3:2 `CardsOfValue` v2:_ ->
      FullHouse (ThreeOfAKind v3) (Pair v2)
    3 `CardsOfValue` v:_ -> ThreeOfAKindH $ ThreeOfAKind v
    2 `CardsOfValue` v1:2 `CardsOfValue` v2:_ -> TwoPairH (Pair v1) (Pair v2)
    2 `CardsOfValue` v:_ -> PairH $ Pair v
    1 `CardsOfValue` v:_ -> HighCard v

scoreHand :: [Card] -> Hand
scoreHand cards = maximum $ concatMap ($ cards) scorers
  where
    scorers = [(: []) . scoreHandForSameRank, makeStraights, makeFlushes]
