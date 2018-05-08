module Main where

  import StringAlignment
  import Test.HUnit

  similarityScoreTest =
    test
    [ similarityScore "writers" "vintner" ~?= -5
    , similarityScore "a" "" ~?= (1 * scoreMismatch)
    , similarityScore "" "aa" ~?= (2 * scoreMismatch)
    , similarityScore "" "" ~?= 0 ]

  main = runTestTT $
    test [
      "similarityScore" ~: Main.similarityScoreTest
    ]
