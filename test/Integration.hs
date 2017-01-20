{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import           Control.Exception.Base
import           Data.Foldable
import qualified Data.Map.Strict        as M
import           Data.Monoid
import           Database.Bolt          hiding (Value)
import           System.Timeout
import           Test.Hspec

main :: IO ()
main = hspec $ do test1
                  test2

aROUND :: (Pipe -> IO a) -> IO a
aROUND = bracket open close

seconds :: Int
seconds = 10^(6::Integer)

open :: IO Pipe
open = connect $ def {user = "neo4j", password = "neo4j"}

test1 :: Spec
test1 =
  describe "Test for unexpected, unexplainable halting sometimes for queries that lack their parameters" $
    it "(tests a timeout of 5 seconds)" $
      aROUND (timeout (5*seconds) . (`run` queryP "MATCH (n {prop: {prop}}) RETURN n;" M.empty))
       `shouldReturn` Just []

test2 :: Spec
test2 =
  describe "unsafe functions in Responses" $
    it "(test needs to run at least twice to show result)" $
      aROUND ( forM_ [ query_ "MATCH (t:Test) delete t;"
                     , query_ "CREATE (t:Test {testID:1});"
                     , query_ "CREATE (t:Test {testID:2});"
                     , query_ $ "MATCH (t1:Test {testID:1}),(t2:Test {testID:2})" <>
                                "CREATE (t1)-[r:Relation]->(t2) return r"
                     ] . run
             ) `shouldReturn` ()
