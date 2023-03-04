module Main where

import Polysemy.Account.Api.Test.AccountWebTest (
  test_failLoginUser,
  test_loginUser,
  test_registerFailUser,
  test_registerUser,
  )
import Polysemy.Account.Api.Test.AuthTest (test_authApi, test_autoAuth)
import Polysemy.Account.Api.Test.RegisterTest (test_register)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)


tests :: TestTree
tests =
  testGroup "all" [
    unitTest "successful login" test_loginUser,
    unitTest "login with wrong password" test_failLoginUser,
    unitTest "successful register" test_registerUser,
    unitTest "register existing account name" test_registerFailUser,
    unitTest "obtain an auth token" test_authApi,
    unitTest "create user when making test request" test_autoAuth,
    unitTest "register an account" test_register
  ]

main :: IO ()
main =
  defaultMain tests
