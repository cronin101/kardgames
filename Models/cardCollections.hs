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
      TableState (arrangeTableCards $ take 3 deck) $ drop 3 deck
    TableState flopOrTurn deck -> dealOne flopOrTurn deck
  where
    dealOne tableCards deck =
      TableState
        (arrangeTableCards $ sequenceTableCards tableCards ++ [head deck]) $
      drop 1 deck

arrangeTableCards :: [Card] -> TableCards
arrangeTableCards cards
  | length cards < 3 = NoTableCards
  | length cards == 3 = PostFlop $ Flop (head cards) (cards !! 1) (cards !! 2)
  | length cards == 4 =
    PostTurn $ Turn (flop . arrangeTableCards $ take 3 cards) (cards !! 3)
  | length cards == 5 =
    PostRiver $ River (turn . arrangeTableCards $ take 4 cards) (cards !! 4)
  | otherwise = NoTableCards

sequenceTableCards :: TableCards -> [Card]
sequenceTableCards tableCards =
  case tableCards of
    NoTableCards              -> []
    PostFlop (Flop c1 c2 c3)  -> [c1, c2, c3]
    PostTurn (Turn flop c4)   -> sequenceTableCards (PostFlop flop) ++ [c4]
    PostRiver (River turn c5) -> sequenceTableCards (PostTurn turn) ++ [c5]
