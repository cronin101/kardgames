module CardsTests where

import           Models.CardCollections
import           Models.Cards
import           Models.Hands
import           Test.HUnit

tenOfSpades = Card Spade (Rank Ten)

tenOfHearts = Card Heart (Rank Ten)

tenOfDiamonds = Card Diamond (Rank Ten)

tenOfClubs = Card Club (Rank Ten)

nineOfSpades = Card Spade (Rank Nine)

aceOfSpades = Card Spade (Face Ace)

tests =
  TestList
    [ TestLabel "Ten is greater than Nine" $
      TestCase $ assert $ tenOfSpades > nineOfSpades
    , TestLabel "Ten is equal to Ten, regardless of suit" $
      TestCase $ assert $ tenOfSpades == tenOfHearts
    , TestLabel "Dealing turns an empty table into a flop" $
      TestCase $
      assertEqual
        "Same three cards on the table"
        (deal $ TableState NoTableCards [tenOfSpades, tenOfHearts, nineOfSpades])
        (TableState
           (arrangeTableCards [tenOfSpades, tenOfHearts, nineOfSpades])
           [])
    , TestLabel "Dealing turns an flop into a turn" $
      TestCase $
      assertEqual
        "Same four cards on the table"
        (deal $
         TableState
           (arrangeTableCards [tenOfSpades, tenOfHearts, nineOfSpades])
           [tenOfDiamonds])
        (TableState
           (arrangeTableCards
              [tenOfSpades, tenOfHearts, nineOfSpades, tenOfDiamonds])
           [])
    , TestLabel "Dealing turns an turn into a river" $
      TestCase $
      assertEqual
        "Same five cards on the table"
        (deal $
         TableState
           (arrangeTableCards
              [tenOfSpades, tenOfHearts, nineOfSpades, tenOfDiamonds])
           [tenOfClubs])
        (TableState
           (arrangeTableCards
              [ tenOfSpades
              , tenOfHearts
              , nineOfSpades
              , tenOfDiamonds
              , tenOfClubs
              ])
           [])
    , TestLabel "We can detect the largest number of same-value cards :: 1 card" $
      TestCase $
      assertEqual
        "One card"
        (findHighestCountOfSameRank [tenOfSpades])
        (1 `CardsOfValue` Rank Ten)
    , TestLabel
        "We can detect the largest number of same-value cards :: 2 cards" $
      TestCase $
      assertEqual
        "Two cards"
        (findHighestCountOfSameRank [tenOfSpades, tenOfClubs])
        (2 `CardsOfValue` Rank Ten)
    , TestLabel
        "We can detect the largest number of same-value cards :: 2 cards with noise" $
      TestCase $
      assertEqual
        "Two cards with noise"
        (findHighestCountOfSameRank
           [tenOfSpades, nineOfSpades, tenOfClubs, aceOfSpades])
        (2 `CardsOfValue` Rank Ten)
    , TestLabel "Single cards are scored on value" $
      TestCase $
      assert $ 1 `CardsOfValue` Rank Two < 1 `CardsOfValue` Rank Three
    , TestLabel "Two cards are better than one" $
      TestCase $
      assert $ 2 `CardsOfValue` Rank Two > 1 `CardsOfValue` Rank Three
    , TestLabel "Higher card beats high card" $
      TestCase $ assert $ HighCard (Face Ace) > HighCard (Face King)
    , TestLabel "Pair beats high card" $
      TestCase $
      assert $ PairH (Pair (Face King) (Heart, Diamond)) > HighCard (Face Ace)
    , TestLabel "Higher pair beats pair" $
      TestCase $
      assert $
      PairH (Pair (Face Ace) (Heart, Diamond)) >
      PairH (Pair (Face King) (Heart, Diamond))
    , TestLabel "Triple beats pair" $
      TestCase $
      assert $
      ThreeOfAKindH (ThreeOfAKind (Face King) (Heart, Diamond, Spade)) >
      PairH (Pair (Face Ace) (Heart, Diamond))
    , TestLabel "Higher triple beats triple" $
      TestCase $
      assert $
      ThreeOfAKindH (ThreeOfAKind (Face Ace) (Heart, Diamond, Spade)) >
      ThreeOfAKindH (ThreeOfAKind (Face King) (Heart, Diamond, Spade))
    , TestLabel "Straight beats triple" $
      TestCase $
      assert $
      Straight (Face King) >
      ThreeOfAKindH (ThreeOfAKind (Face Ace) (Heart, Diamond, Spade))
    , TestLabel "Higher straight beats straight" $
      TestCase $ assert $ Straight (Face Ace) > Straight (Face King)
    , TestLabel "Flush beats straight" $
      TestCase $ assert $ Flush Heart (Face King) > Straight (Face Ace)
    , TestLabel "Higher flush beats flush" $
      TestCase $ assert $ Flush Heart (Face Ace) > Flush Heart (Face King)
    , TestLabel "Full house beats flush" $
      TestCase $
      assert $
      FullHouse
        (ThreeOfAKind (Face King) (Heart, Diamond, Spade))
        (Pair (Face Queen) (Heart, Diamond)) >
      Flush Heart (Face King)
    , TestLabel "Higher full house beats full house" $
      TestCase $
      assert $
      FullHouse
        (ThreeOfAKind (Face Ace) (Heart, Diamond, Spade))
        (Pair (Face King) (Heart, Diamond)) >
      FullHouse
        (ThreeOfAKind (Face King) (Heart, Diamond, Spade))
        (Pair (Face Queen) (Heart, Diamond))
    , TestLabel "Four of a kind beats full house" $
      TestCase $
      assert $
      FourOfAKind (Rank Ten) >
      FullHouse
        (ThreeOfAKind (Face King) (Heart, Diamond, Spade))
        (Pair (Face Queen) (Heart, Diamond))
    , TestLabel "Higher four of a kind beats four of a kind" $
      TestCase $ assert $ FourOfAKind (Face Ace) > FourOfAKind (Face King)
    , TestLabel "Straight flush beats four of a kind" $
      TestCase $
      assert $ StraightFlush Heart (Face King) > FourOfAKind (Face Ace)
    , TestLabel "Higher straight flush beats straight flush" $
      TestCase $
      assert $ StraightFlush Heart (Face Ace) > StraightFlush Heart (Face King)
    ]

main = runTestTT tests
