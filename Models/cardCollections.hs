module Models.CardCollections where

import           Models.Cards

data Hole =
  Hole Card Card
  deriving (Show, Read, Eq)

data Flop =
  Flop Card Card Card
  deriving (Show, Read, Eq)

data Turn =
  Turn
    { existingFlop :: Flop
    , turnCard     :: Card
    }
  deriving (Show, Read, Eq)

data River =
  River
    { existingTurn :: Turn
    , riverCard    :: Card
    }
  deriving (Show, Read, Eq)

data TableCards
  = NoTableCards
  | PostFlop
      { flop :: Flop
      }
  | PostTurn
      { turn :: Turn
      }
  | PostRiver
      { river :: River
      }
  deriving (Show, Read, Eq)

data TableState =
  TableState
    { visibleCards :: TableCards
    , deck         :: [Card]
    }
  deriving (Show, Read, Eq)

deal :: TableState -> TableState
deal ts =
  case ts of
    TableState NoTableCards deck ->
      TableState (PostFlop $ flopDeck deck) $ drop 3 deck
    TableState (PostFlop flop) deck ->
      TableState (PostTurn $ Turn flop (head deck)) $ drop 1 deck
    TableState (PostTurn turn) deck ->
      TableState (PostRiver $ River turn (head deck)) $ drop 1 deck
  where
    flopDeck deck = Flop (head deck) (deck !! 1) (deck !! 2)

arrangeTableCards :: [Card] -> TableCards
arrangeTableCards cards
  | length cards < 3 = NoTableCards
  | length cards == 3 = PostFlop $ Flop (head cards) (cards !! 1) (cards !! 2)
  | length cards == 4 =
    PostTurn $ Turn (flop . arrangeTableCards $ take 3 cards) (cards !! 3)
  | length cards == 5 =
    PostRiver $ River (turn . arrangeTableCards $ take 4 cards) (cards !! 4)
  | otherwise = NoTableCards
