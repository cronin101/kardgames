module CardsTests where

import           Models.CardCollections
import           Models.Cards
import           Test.HUnit

tenOfSpades = Card Spade (Rank Ten)

tenOfHearts = Card Heart (Rank Ten)

tenOfDiamonds = Card Diamond (Rank Ten)

tenOfClubs = Card Club (Rank Ten)

nineOfSpades = Card Spade (Rank Nine)

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
    ]

main = runTestTT tests
