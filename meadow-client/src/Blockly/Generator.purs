module Blockly.Generator where

import Prelude
import Blockly.Types (Block, BlocklyState, Workspace)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn4, Fn5, Fn6, runFn1, runFn4, runFn5, runFn6)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Partial.Unsafe (unsafePartial)

type GeneratorFunction
  = BlocklyState -> Block -> Either String String

data Order
  = Atomic
  | None

toNumber :: Order -> Number
toNumber Atomic = 0.0

toNumber None = 99.0

foreign import data Generator :: Type

foreign import data Input :: Type

foreign import data Field :: Type

foreign import getFieldValue_ :: forall a. Fn4 (String -> Either String a) (a -> Either String a) Block String (Either String String)

foreign import statementToCode_ :: forall a. Fn5 (String -> Either String a) (a -> Either String a) Generator Block String (Either String String)

foreign import valueToCode_ :: forall a. Fn6 (String -> Either String a) (a -> Either String a) Generator Block String Number (Either String String)

foreign import mkGenerator_ :: EffectFn2 BlocklyState String Generator

foreign import insertGeneratorFunction_ :: EffectFn4 BlocklyState Generator String (BlocklyState -> Block -> String) Unit

foreign import workspaceToCode_ :: EffectFn2 BlocklyState Generator String

foreign import inputList_ :: Fn1 Block (Array Input)

foreign import connectToPrevious_ :: EffectFn2 Block Input Unit

foreign import connectToOutput_ :: EffectFn2 Block Input Unit

foreign import newBlock_ :: EffectFn2 Workspace String Block

foreign import inputName_ :: Fn1 Input String

foreign import clearWorkspace_ :: EffectFn1 Workspace Unit

foreign import fieldRow_ :: Fn1 Input (Array Field)

foreign import setFieldText_ :: EffectFn2 Field String Unit

foreign import fieldName_ :: Fn1 Field String

foreign import unsafeThrowError_ :: forall a. Fn1 String a

getFieldValue :: Block -> String -> Either String String
getFieldValue = runFn4 getFieldValue_ Left Right

statementToCode :: Generator -> Block -> String -> Either String String
statementToCode = runFn5 statementToCode_ Left Right

valueToCode :: Generator -> Block -> String -> Order -> Either String String
valueToCode g b v o = runFn6 valueToCode_ Left Right g b v (toNumber o)

mkGenerator :: BlocklyState -> String -> Effect Generator
mkGenerator = runEffectFn2 mkGenerator_

insertGeneratorFunction :: BlocklyState -> Generator -> String -> GeneratorFunction -> Effect Unit
insertGeneratorFunction bs g k f = runEffectFn4 insertGeneratorFunction_ bs g k (compose2 (unsafePartial unsafeFromRight) f)

unsafeFromRight :: forall a. Partial => Either String a -> a
unsafeFromRight (Right a) = a

unsafeFromRight (Left e) = runFn1 unsafeThrowError_ e

compose2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (<<<) <<< (<<<)

workspaceToCode :: BlocklyState -> Generator -> Effect String
workspaceToCode = runEffectFn2 workspaceToCode_

inputList :: Block -> Array Input
inputList = runFn1 inputList_

connectToPrevious :: Block -> Input -> Effect Unit
connectToPrevious = runEffectFn2 connectToPrevious_

connectToOutput :: Block -> Input -> Effect Unit
connectToOutput = runEffectFn2 connectToOutput_

newBlock :: Workspace -> String -> Effect Block
newBlock = runEffectFn2 newBlock_

inputName :: Input -> String
inputName = runFn1 inputName_

getInputWithName :: Array Input -> String -> Maybe Input
getInputWithName inputs name = do
  idx <- Array.findIndex (\i -> (inputName i) == name) inputs
  Array.index inputs idx

clearWorkspace :: Workspace -> Effect Unit
clearWorkspace = runEffectFn1 clearWorkspace_

fieldRow :: Input -> Array Field
fieldRow = runFn1 fieldRow_

setFieldText :: Field -> String -> Effect Unit
setFieldText = runEffectFn2 setFieldText_

fieldName :: Field -> String
fieldName = runFn1 fieldName_
