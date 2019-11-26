module CardTypesTests where

import           CardTypes
import           Test.HUnit

tests =
  TestList
    [ TestLabel "Ten is greater than Nine" $
      TestCase $ assert $ Card Spade (Rank Ten) > Card Spade (Rank Nine)
    , TestLabel "Ten is equal to Ten, regardless of suit" $
      TestCase $ assert $ Card Spade (Rank Ten) == Card Heart (Rank Ten)
    ]

main = runTestTT tests
