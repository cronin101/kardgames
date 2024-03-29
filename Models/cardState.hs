module Models.CardState where

import           Models.Cards
import           Models.Hands

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

data CardState
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
    { visibleCards :: CardState
    , deck         :: [Card]
    }
  deriving (Show, Read, Eq)

data HandStrength
  = Provisional Hand Hole
  | Final Hand Hole

deal :: TableState -> TableState
deal ts =
  case ts of
    TableState NoTableCards deck ->
      TableState (cardsToCardState $ take 3 deck) $ drop 3 deck
    TableState flopOrTurn deck -> dealOne flopOrTurn deck
  where
    dealOne tableCards (newCard:restOfDeck) =
      TableState
        (cardsToCardState $ cardStateToCards tableCards ++ [newCard])
        restOfDeck

cardsToCardState :: [Card] -> CardState
cardsToCardState cards =
  case reverse cards of
    [c3, c2, c1]       -> PostFlop $ Flop c1 c2 c3
    c4:cs@[_, _, _]    -> PostTurn $ Turn (flop . cardsToCardState $ cs) c4
    c5:cs@[_, _, _, _] -> PostRiver $ River (turn . cardsToCardState $ cs) c5
    _                  -> NoTableCards

cardStateToCards :: CardState -> [Card]
cardStateToCards = reverse . cardStateToReversedCards
  where
    cardStateToReversedCards cardState =
      case cardState of
        NoTableCards              -> []
        PostFlop (Flop c1 c2 c3)  -> [c3, c2, c1]
        PostTurn (Turn flop c4)   -> c4 : cardStateToCards (PostFlop flop)
        PostRiver (River turn c5) -> c5 : cardStateToCards (PostTurn turn)

calculateHandStrength :: TableState -> Hole -> HandStrength
calculateHandStrength ts h =
  case ts of
    TableState riverState@(PostRiver _) _ ->
      Final (scoreHand (cardStateToCards riverState ++ getHole h)) h
    TableState cardState _ ->
      Provisional (scoreHand (cardStateToCards cardState ++ getHole h)) h
  where
    getHole (Hole a b) = [a, b]
