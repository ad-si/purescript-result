module Data.Result where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Extend (class Extend)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Either (Either(Left, Right))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)

-- | The `Result` type is used to represent computations
-- | that may succeed or fail.
data Result error value = Error error | Ok value

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Ok` with the `<$>` operator:
-- |
-- | ``` purescript
-- | f <$> Ok x == Ok (f x)
-- | ```
-- |
-- | `Error` values are untouched:
-- |
-- | ``` purescript
-- | f <$> Error y == Error y
-- | ```
derive instance functorResult :: Functor (Result a)

instance invariantResult :: Invariant (Result a) where
  imap = imapF

instance bifunctorResult :: Bifunctor Result where
  bimap f _ (Error l) = Error (f l)
  bimap _ g (Ok r) = Ok (g r)

-- | The `Apply` instance allows functions contained within a `Ok` to
-- | transform a value contained within a `Ok` using the `(<*>)` operator:
-- |
-- | ``` purescript
-- | Ok f <*> Ok x == Ok (f x)
-- | ```
-- |
-- | `Error` values are left untouched:
-- |
-- | ``` purescript
-- | Error f <*> Ok x == Error x
-- | Ok f <*> Error y == Error y
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used to transform a
-- | pure function to take `Result`-typed arguments so `f :: a -> b -> c`
-- | becomes `f :: Result l a -> Result l b -> Result l c`:
-- |
-- | ``` purescript
-- | f <$> Ok x <*> Ok y == Ok (f x y)
-- | ```
-- |
-- | The `Error`-preserving behaviour of both operators means the result of
-- | an expression like the above but where any one of the values is `Error`
-- | means the whole result becomes `Error` also, taking the first `Error` value
-- | found:
-- |
-- | ``` purescript
-- | f <$> Error x <*> Ok y == Error x
-- | f <$> Ok x <*> Error y == Error y
-- | f <$> Error x <*> Error y == Error x
-- | ```
instance applyResult :: Apply (Result e) where
  apply (Error e) _ = Error e
  apply (Ok f) r = f <$> r

-- | The `Applicative` instance enables lifting of values into `Result` with the
-- | `pure` function:
-- |
-- | ``` purescript
-- | pure x :: Result _ _ == Ok x
-- | ```
-- |
-- | Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
-- | `pure` can be used to pass a mixture of `Result` and non-`Result` typed
-- | values to a function that does not usually expect them, by using `pure`
-- | for any value that is not already `Result` typed:
-- |
-- | ``` purescript
-- | f <$> Ok x <*> pure y == Ok (f x y)
-- | ```
-- |
-- | Even though `pure = Ok` it is recommended to use `pure` in situations
-- | like this as it allows the choice of `Applicative` to be changed later
-- | without having to go through and replace `Ok` with a new constructor.
instance applicativeResult :: Applicative (Result e) where
  pure = Ok

-- | The `Alt` instance allows for a choice to be made between two `Result`
-- | values with the `<|>` operator, where the first `Ok` encountered
-- | is taken.
-- |
-- | ``` purescript
-- | Ok x <|> Ok y == Ok x
-- | Error x <|> Ok y == Ok y
-- | Error x <|> Error y == Error y
-- | ```
instance altResult :: Alt (Result e) where
  alt (Error _) r = r
  alt l        _ = l

-- | The `Bind` instance allows sequencing of `Result` values and functions that
-- | return an `Result` by using the `>>=` operator:
-- |
-- | ``` purescript
-- | Error x >>= f = Error x
-- | Ok x >>= f = f x
-- | ```
instance bindResult :: Bind (Result e) where
  bind = result (\e _ -> Error e) (\a f -> f a)

-- | The `Monad` instance guarantees that there are both `Applicative` and
-- | `Bind` instances for `Result`. This also enables the `do` syntactic sugar:
-- |
-- | ``` purescript
-- | do
-- |   x' <- x
-- |   y' <- y
-- |   pure (f x' y')
-- | ```
-- |
-- | Which is equivalent to:
-- |
-- | ``` purescript
-- | x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
-- | ```
instance monadResult :: Monad (Result e)

-- | The `Extend` instance allows sequencing of `Result` values and functions
-- | that accept an `Result` and return a non-`Result` result using the
-- | `<<=` operator.
-- |
-- | ``` purescript
-- | f <<= Error x = Error x
-- | f <<= Ok x = Ok (f (Ok x))
-- | ```
instance extendResult :: Extend (Result e) where
  extend _ (Error y)  = Error y
  extend f x         = Ok (f x)

-- | The `Show` instance allows `Result` values to be rendered as a string with
-- | `show` whenever there is an `Show` instance for both type the `Result` can
-- | contain.
instance showResult :: (Show a, Show b) => Show (Result a b) where
  show (Error x) = "(Error " <> show x <> ")"
  show (Ok y) = "(Ok " <> show y <> ")"

-- | The `Eq` instance allows `Result` values to be checked for equality with
-- | `==` and inequality with `/=` whenever there is an `Eq` instance for both
-- | types the `Result` can contain.
derive instance eqResult :: (Eq a, Eq b) => Eq (Result a b)

derive instance eq1Result :: Eq a => Eq1 (Result a)

-- | The `Ord` instance allows `Result` values to be compared with
-- | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
-- | both types the `Result` can contain.
-- |
-- | Any `Error` value is considered to be less than a `Ok` value.
derive instance ordResult :: (Ord a, Ord b) => Ord (Result a b)

derive instance ord1Result :: Ord a => Ord1 (Result a)

instance boundedResult :: (Bounded a, Bounded b) => Bounded (Result a b) where
  top = Ok top
  bottom = Error bottom

instance foldableResult :: Foldable (Result a) where
  foldr _ z (Error _)  = z
  foldr f z (Ok x) = f x z
  foldl _ z (Error _)  = z
  foldl f z (Ok x) = f z x
  foldMap f (Error _)  = mempty
  foldMap f (Ok x) = f x

instance bifoldableResult :: Bifoldable Result where
  bifoldr f _ z (Error a) = f a z
  bifoldr _ g z (Ok b) = g b z
  bifoldl f _ z (Error a) = f z a
  bifoldl _ g z (Ok b) = g z b
  bifoldMap f _ (Error a) = f a
  bifoldMap _ g (Ok b) = g b

instance traversableResult :: Traversable (Result a) where
  traverse _ (Error x)  = pure (Error x)
  traverse f (Ok x) = Ok <$> f x
  sequence (Error x) = pure (Error x)
  sequence (Ok x)  = Ok <$> x

instance bitraversableResult :: Bitraversable Result where
  bitraverse f _ (Error a) = Error <$> f a
  bitraverse _ g (Ok b) = Ok <$> g b
  bisequence (Error a) = Error <$> a
  bisequence (Ok b) = Ok <$> b

instance semigroupResult :: (Semigroup b) => Semigroup (Result a b) where
  append x y = append <$> x <*> y


-- | Convert an `Either` to a `Result`.
-- |
-- | ```purescript
-- | fromEither (Left "error") == Error "error"
-- | fromEither (Right "value") == Ok "value"
-- | ```
fromEither :: forall e v. Either e v -> Result e v
fromEither either = case either of
  Left error -> Error error
  Right value -> Ok value

-- | Convert a `Result` to an `Either`.
-- |
-- | ```purescript
-- | toEither (Error "error") == Left "error"
-- | toEither (Ok "value") == Right "value"
-- | ```
toEither :: forall e v. Result e v -> Either e v
toEither aResult = case aResult of
  Error error -> Left error
  Ok value -> Right value


-- | Takes two functions and an `Result` value, if the value is a `Error` the
-- | inner value is applied to the first function, if the value is a `Ok`
-- | the inner value is applied to the second function.
-- |
-- | ``` purescript
-- | Result f g (Error x) == f x
-- | Result f g (Ok y) == g y
-- | ```
result :: forall a b c. (a -> c) -> (b -> c) -> Result a b -> c
result f _ (Error a) = f a
result _ g (Ok b) = g b

-- | Combine two alternatives.
choose :: forall m a b. Alt m => m a -> m b -> m (Result a b)
choose a b = Error <$> a <|> Ok <$> b

-- | Returns `true` when the `Result` value was constructed with `Error`.
isError :: forall a b. Result a b -> Boolean
isError = result (const true) (const false)

-- | Returns `true` when the `Result` value was constructed with `Ok`.
isOk :: forall a b. Result a b -> Boolean
isOk = result (const false) (const true)

-- | A partial function that extracts the value
-- | from the `Error` data constructor.
-- | Passing a `Ok` to `fromError` will throw an error at runtime.
fromError :: forall a b. Partial => Result a b -> a
fromError (Error a) = a

-- | A partial function that extracts the value from the `Ok` data constructor.
-- | Passing a `Error` to `fromOk` will throw an error at runtime.
fromOk :: forall a b. Partial => Result a b -> b
fromOk (Ok a) = a

-- | Takes a default and a `Maybe` value, if the value is a `Just`, turn it into
-- | a `Ok`, if the value is a `Nothing` use the provided default as a `Error`
-- |
-- | ```purescript
-- | note "default" Nothing = Error "default"
-- | note "default" (Just 1) = Ok 1
-- | ```
note :: forall a b. a -> Maybe b -> Result a b
note a = maybe (Error a) Ok

-- | Turns an `Result` into a `Maybe`,
-- | by throwing eventual `Error` values away and converting
-- | them into `Nothing`.
-- | `Ok` values get turned into `Just`s.
-- |
-- | ```purescript
-- | hush (Error "ParseError") = Nothing
-- | hush (Ok 42) = Just 42
-- | ```
hush :: forall a b. Result a b -> Maybe b
hush = result (const Nothing) Just
