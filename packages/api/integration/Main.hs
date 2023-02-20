module Main where

import Polysemy.Account.Api.Test.AccountByNameTest (test_accountByName)
import Polysemy.Account.Api.Test.AuthForAccountTest (test_authForAccount)
import Polysemy.Account.Api.Test.JwkTest (test_jwk)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "auth for account query" test_authForAccount,
    unitTest "account by name query" test_accountByName,
    unitTest "jwk table" test_jwk
  ]

main :: IO ()
main =
  defaultMain tests
