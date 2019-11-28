module CardsTests where

import           Models.CardCollections
import           Models.Cards
import           Models.Hands
import           Test.HUnit

tests =
  TestList
    [ TestLabel "Ten is greater than Nine" $
      TestCase $ assert $ Card Spade Ten > Card Spade Nine
    , TestLabel "Ten is equal to Ten, regardless of suit" $
      TestCase $ assert $ compare (Card Spade Ten) (Card Heart Ten) == EQ
    , TestLabel "Dealing turns an empty table into a flop" $
      TestCase $
      assertEqual
        "Same three cards on the table"
        (deal $
         TableState
           NoTableCards
           [Card Spade Ten, Card Heart Ten, Card Spade Nine])
        (TableState
           (cardsToCardState [Card Spade Ten, Card Heart Ten, Card Spade Nine])
           [])
    , TestLabel "Dealing turns an flop into a turn" $
      TestCase $
      assertEqual
        "Same four cards on the table"
        (deal $
         TableState
           (cardsToCardState [Card Spade Ten, Card Heart Ten, Card Spade Nine])
           [Card Diamond Ten])
        (TableState
           (cardsToCardState
              [ Card Spade Ten
              , Card Heart Ten
              , Card Spade Nine
              , Card Diamond Ten
              ])
           [])
    , TestLabel "Dealing turns an turn into a river" $
      TestCase $
      assertEqual
        "Same five cards on the table"
        (deal $
         TableState
           (cardsToCardState
              [ Card Spade Ten
              , Card Heart Ten
              , Card Spade Nine
              , Card Diamond Ten
              ])
           [Card Club Ten])
        (TableState
           (cardsToCardState
              [ Card Spade Ten
              , Card Heart Ten
              , Card Spade Nine
              , Card Diamond Ten
              , Card Club Ten
              ])
           [])
    , TestLabel "Single cards are scored on value" $
      TestCase $ assert $ 1 `CardsOfValue` Two < 1 `CardsOfValue` Three
    , TestLabel "Two cards are better than one" $
      TestCase $ assert $ 2 `CardsOfValue` Two > 1 `CardsOfValue` Three
    , TestLabel "Higher card beats high card" $
      TestCase $ assert $ HighCard Ace > HighCard King
    , TestLabel "Pair beats high card" $
      TestCase $ assert $ PairH (Pair King (Heart, Diamond)) > HighCard Ace
    , TestLabel "Higher pair beats pair" $
      TestCase $
      assert $
      PairH (Pair Ace (Heart, Diamond)) > PairH (Pair King (Heart, Diamond))
    , TestLabel "Triple beats pair" $
      TestCase $
      assert $
      ThreeOfAKindH (ThreeOfAKind King (Heart, Diamond, Spade)) >
      PairH (Pair Ace (Heart, Diamond))
    , TestLabel "Higher triple beats triple" $
      TestCase $
      assert $
      ThreeOfAKindH (ThreeOfAKind Ace (Heart, Diamond, Spade)) >
      ThreeOfAKindH (ThreeOfAKind King (Heart, Diamond, Spade))
    , TestLabel "Straight beats triple" $
      TestCase $
      assert $
      Straight King > ThreeOfAKindH (ThreeOfAKind Ace (Heart, Diamond, Spade))
    , TestLabel "Higher straight beats straight" $
      TestCase $ assert $ Straight Ace > Straight King
    , TestLabel "Flush beats straight" $
      TestCase $ assert $ Flush Heart King > Straight Ace
    , TestLabel "Higher flush beats flush" $
      TestCase $ assert $ Flush Heart Ace > Flush Heart King
    , TestLabel "Full house beats flush" $
      TestCase $
      assert $
      FullHouse
        (ThreeOfAKind King (Heart, Diamond, Spade))
        (Pair Queen (Heart, Diamond)) >
      Flush Heart King
    , TestLabel "Higher full house beats full house" $
      TestCase $
      assert $
      FullHouse
        (ThreeOfAKind Ace (Heart, Diamond, Spade))
        (Pair King (Heart, Diamond)) >
      FullHouse
        (ThreeOfAKind King (Heart, Diamond, Spade))
        (Pair Queen (Heart, Diamond))
    , TestLabel "Four of a kind beats full house" $
      TestCase $
      assert $
      FourOfAKind Ten >
      FullHouse
        (ThreeOfAKind King (Heart, Diamond, Spade))
        (Pair Queen (Heart, Diamond))
    , TestLabel "Higher four of a kind beats four of a kind" $
      TestCase $ assert $ FourOfAKind Ace > FourOfAKind King
    , TestLabel "Straight flush beats four of a kind" $
      TestCase $ assert $ StraightFlush Heart King > FourOfAKind Ace
    , TestLabel "Higher straight flush beats straight flush" $
      TestCase $ assert $ StraightFlush Heart Ace > StraightFlush Heart King
    , TestLabel "It can recognise an Ace high straight" $
      TestCase $
      assertEqual
        "Ace high straight"
        (head
           (makeStraights
              [ Card Heart King
              , Card Spade Ace
              , Card Diamond Queen
              , Card Spade Jack
              , Card Spade Ten
              ]))
        (Straight Ace)
    , TestLabel "It can recognise an Ace low straight" $
      TestCase $
      assertEqual
        "Ace low straight"
        (head
           (makeStraights
              [ Card Heart Two
              , Card Spade Ace
              , Card Diamond Four
              , Card Spade Five
              , Card Spade Three
              ]))
        (Straight Five)
    , TestLabel "It can recognise a straight flush" $
      TestCase $
      assertEqual
        "straight flush"
        (head $
         makeStraights
           [ Card Heart King
           , Card Heart Ace
           , Card Heart Queen
           , Card Heart Jack
           , Card Heart Ten
           ])
        (StraightFlush Heart Ace)
    , TestLabel "It can recognise a flush" $
      TestCase $
      assertEqual
        "flush"
        (head $
         makeFlushes
           [ Card Spade Two
           , Card Spade Ten
           , Card Spade Five
           , Card Spade Queen
           , Card Spade Four
           ])
        (Flush Spade Queen)
    ]

main = runTestTT tests
