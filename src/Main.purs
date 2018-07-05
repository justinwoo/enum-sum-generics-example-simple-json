module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.String (toLower)
import Effect (Effect)
import Effect.Console (log)
import Foreign (F, Foreign, ForeignError(ForeignError))
import Simple.JSON (class ReadForeign, read', readJSON)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

enumReadForeignLowercase :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> F a
enumReadForeignLowercase f =
  to <$> enumReadForeignImpl (StringTransform toLower) f

newtype StringTransform = StringTransform (String -> String)

-- type class for "enums", or nullary sum types
class EnumReadForeign rep where
  enumReadForeignImpl :: StringTransform -> Foreign -> F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl fn f
      = Inl <$> enumReadForeignImpl fn f
    <|> Inr <$> enumReadForeignImpl fn f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl (StringTransform fn) f = do
    s <- read' f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = fn $ reflectSymbol (SProxy :: SProxy name)

data Fruit
  = Abogado
  | Boat
  | Candy
derive instance genericFruit :: Generic Fruit _
instance fruitReadForeign :: ReadForeign Fruit where
  readImpl = enumReadForeignLowercase
instance furitShow :: Show Fruit where
  show = genericShow

type MyThing =
  { fruit :: Fruit
  }

testJSON1 :: String
testJSON1 = """
{
  "fruit": "abogado"
}
"""

testJSON2 :: String
testJSON2 = """
{
  "fruit": "Abogado"
}
"""

testJSON3 :: String
testJSON3 = """
{
  "fruit": "Wrongthing"
}
"""

main :: Effect Unit
main = do
  logJSON $ readJSON testJSON1
  logJSON $ readJSON testJSON2
  logJSON $ readJSON testJSON3
  -- output:
  -- Abogado
  -- (NonEmptyList (NonEmpty (ErrorAtProperty "fruit" (ForeignError "Enum string Abogado did not match expected string abogado")) ((ErrorAtProperty "fruit" (ForeignError "Enum string Abogado did not match expected string boat")) : (ErrorAtProperty "fruit" (ForeignError "Enum string Abogado did not match expected string candy")) : Nil)))
  -- (NonEmptyList (NonEmpty (ErrorAtProperty "fruit" (ForeignError "Enum string Wrongthing did not match expected string abogado")) ((ErrorAtProperty "fruit" (ForeignError "Enum string Wrongthing did not match expected string boat")) : (ErrorAtProperty "fruit" (ForeignError "Enum string Wrongthing did not match expected string candy")) : Nil)))
  where
    toString :: Either (NonEmptyList ForeignError) MyThing -> String
    toString = either show (show <<< _.fruit)
    logJSON = log <<< toString
