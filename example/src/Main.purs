module Main where

import Prelude (Unit, show, (#), ($), (+))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Data.Variant (Variant, case_, on)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)
import Effect.Uncurried (EffectFn2, mkEffectFn2)


foreign import data Margin :: Type

foreign import data Margin2 :: Type -> Type

newtype Test2 = Test2 { "a" :: String, b :: Int, "a-b" :: Boolean }

newtype Test3 = Test3 { r :: String, h :: Boolean, l :: Int }

data Test1 = Test1

data Test4 
  = CA Int 
  | CB String
  | CC Boolean

data Test5
  = CT1 Test1 String
  | CT2 Test2
  | CT3 Test3
  | CT4 Test4


data Test6 a b = Test61 String | Test62 { avi :: a } | Test63 { ver :: Nullable b }
main :: Effect Unit
main = do
  log "Hello sailor!"


identity :: forall a. a -> a
identity a = a

type Avinash a = { avinash :: a, verma :: String}

class A 


class B b

class C c where
  c :: c -> c

-- variantToString :: Variant (num :: Number, str :: String) -> String
-- variantToString = case_ # on (Proxy :: Proxy "num") show
--                         # on (Proxy :: Proxy "str") (\x -> x)

nullableToString :: Nullable String -> String
nullableToString x = case toMaybe x of
  Just y -> y
  Nothing -> "null"

numToSomeObj :: Number -> Object Number
numToSomeObj x = fromFoldable [Tuple "foo" x, Tuple "bar" (x + 1.0)]

someEffectFn :: EffectFn2 Number Number Unit
someEffectFn = mkEffectFn2 $ \a b -> do
  log (show (a + b))

-- Function s t
-- Array t
-- Record { key1 :: Type1, key2 :: Type2 }
-- Number, Int
-- String, Char
-- Boolean
-- Tuple a b
-- Maybe a
-- Either a b
-- Data.Function.Uncurried
-- Effect
-- Control.Monad.Eff
-- Data.StrMap.StrMap
-- Data.Variant
-- Data.Nullable
