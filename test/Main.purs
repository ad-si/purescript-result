module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Either (Either(Left,Right))
import Data.Foldable (and)
import Data.Result (Result(Error,Ok), fromEither, toEither)
import Prelude


aError = Error "error" :: Result String String
aValue = Ok "value" :: Result String String

aLeft = Left "error" :: Either String String
aRight = Right "value" :: Either String String


tests :: Array Boolean
tests =
  [ fromEither aLeft == aError
  , fromEither aRight == aValue
  , toEither aError == aLeft
  , toEither aValue == aRight
  ]


main :: forall eff.
  Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
main =
  if (and tests)
  then log "All tests passed ✔"
  else throw $ "Tests failed: " <> (show tests)
