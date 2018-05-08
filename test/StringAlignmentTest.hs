module Main where

  import StringAlignment
  import Test.HUnit

  similarityScoreTest =
    test
    [ similarityScore "writers" "vintner" ~?= -5
    , similarityScore "a" "" ~?= (1 * scoreMismatch)
    , similarityScore "" "aa" ~?= (2 * scoreMismatch)
    , similarityScore "" "" ~?= 0 ]

  maximaByTest =
    test
    [ maximaBy length ["cs", "efd", "lth", "it"] ~?= ["efd","lth"] ]

  main = runTestTT $
    test
    [ "similarityScore" ~: Main.similarityScoreTest
    , "maximaByTest" ~: Main.maximaByTest ]
