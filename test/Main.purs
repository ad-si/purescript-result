module Test.Main where

import Data.Either (Either(Left,Right))
import Data.Foldable (and)
import Data.Result (Result(Error,Ok), fromEither, toEither)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
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


main :: Effect Unit
main =
  if (and tests)
  then log "All tests passed ✔"
  else throw $ "Tests failed: " <> (show tests)
