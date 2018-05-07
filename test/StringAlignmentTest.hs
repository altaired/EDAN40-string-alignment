module Main where

  import StringAlignment
  import Test.HUnit

  similarityScoreTest =
    test [
      similarityScore "writers" "vintner" ~?= -5
    ]

  main = runTestTT $
    test [
      "similarityScore" ~: Main.similarityScoreTest
    ]
