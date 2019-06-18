module Blockly where

import Prelude

import Blockly.Types (Block, Blockly, BlocklyState, Workspace)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Foldable, traverse_)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Halogen.HTML (AttrName(..), ElemName(..), Node)
import Halogen.HTML.Elements (element)
import Halogen.HTML.Properties (IProp, attr)
import Record as Record
import Simple.JSON (class WriteForeign)
import Simple.JSON as JSON

foreign import createBlocklyInstance_ :: Effect Blockly

foreign import createWorkspace_ :: EffectFn3 Blockly String String Workspace

foreign import resizeBlockly_ :: EffectFn1 BlocklyState Unit

foreign import addBlockType_ :: EffectFn3 BlocklyState String Foreign Unit

foreign import initializeWorkspace_ :: EffectFn2 Blockly Workspace Unit

foreign import render_ :: EffectFn1 Workspace Unit

foreign import getBlockById_ :: forall a. Fn4 (a -> Maybe a) (Maybe a) Workspace String (Maybe Block)

newtype Div
  = Div String

derive instance newtypeDiv :: Newtype Div _

createBlocklyInstance :: Div -> Div -> Effect BlocklyState
createBlocklyInstance workspaceDiv toolboxDiv = do
  blockly <- createBlocklyInstance_
  workspace <- runEffectFn3 createWorkspace_ blockly (unwrap workspaceDiv) (unwrap toolboxDiv)
  pure { blockly, workspace }

resize :: Maybe BlocklyState -> Effect Unit
resize (Just blocklyState) = runEffectFn1 resizeBlockly_ blocklyState

resize Nothing = pure unit

addBlockType :: BlocklyState -> BlockDefinition -> Effect Unit
addBlockType blocklyState (BlockDefinition fields) =
  let
    definition = JSON.write $ Record.delete type_ fields

    type' = fields.type
  in
    runEffectFn3 addBlockType_ blocklyState type' definition

addBlockTypes :: forall f. Foldable f => BlocklyState -> f BlockDefinition -> Effect Unit
addBlockTypes blocklyState = traverse_ (addBlockType blocklyState)

initializeWorkspace :: BlocklyState -> Effect Unit
initializeWorkspace state = runEffectFn2 initializeWorkspace_ state.blockly state.workspace

render :: Workspace -> Effect Unit
render = runEffectFn1 render_

getBlockById :: Workspace -> String -> Maybe Block
getBlockById = runFn4 getBlockById_ Just Nothing

data Pair
  = Pair String String

instance writeForeignPair :: WriteForeign Pair where
  writeImpl (Pair first second) = JSON.write [first, second]

data Arg
  = Input {name :: String, text :: String, spellcheck :: Boolean}
  | Dropdown {name :: String, options :: Array Pair}
  | Checkbox {name :: String, checked :: Boolean}
  | Colour {name :: String, colour :: String}
  | Number {name :: String, value :: Number, min :: Maybe Number, max :: Maybe Number, precision :: Maybe Number}
  | Angle {name :: String, angle :: Number}
  | Variable {name :: String, variable :: String}
  | Date {name :: String, date :: String}
  | Label {text :: Maybe String, class :: Maybe String}
  | Image {src :: String, width :: Number, height :: Number, alt :: String}
  | Value {name :: String, check :: String}
  | Statement {name :: String, check :: String, align :: AlignDirection}
  | Dummy

type_ :: SProxy "type"
type_ = SProxy

instance writeForeignArg :: WriteForeign Arg where
  writeImpl (Input fields) = JSON.write $ Record.insert type_ "field_input" fields
  writeImpl (Dropdown fields) = JSON.write $ Record.insert type_ "field_dropdown" fields
  writeImpl (Checkbox fields) = JSON.write $ Record.insert type_ "field_checkbox" fields
  writeImpl (Colour fields) = JSON.write $ Record.insert type_ "field_colour" fields
  writeImpl (Number fields) = JSON.write $ Record.insert type_ "field_number" fields
  writeImpl (Angle fields) = JSON.write $ Record.insert type_ "field_angle" fields
  writeImpl (Variable fields) = JSON.write $ Record.insert type_ "field_variable" fields
  writeImpl (Date fields) = JSON.write $ Record.insert type_ "field_date" fields
  writeImpl (Label fields) = JSON.write $ Record.insert type_ "field_label" fields
  writeImpl (Image fields) = JSON.write $ Record.insert type_ "field_image" fields
  writeImpl (Value fields) = JSON.write $ Record.insert type_ "input_value" fields
  writeImpl (Statement fields) = JSON.write $ Record.insert type_ "input_statement" fields
  writeImpl Dummy = JSON.write $ {type: "input_dummy"}

data AlignDirection
  = Left
  | Centre
  | Right

instance writeForeignAlignDirection :: WriteForeign AlignDirection where
  writeImpl Left = JSON.write "LEFT"
  writeImpl Centre = JSON.write "CENTRE"
  writeImpl Right = JSON.write "RIGHT"

type BasicBlockDefinition r
  = ( message0 :: String
  , args0 :: Array Arg
  , lastDummyAlign0 :: AlignDirection
  , colour :: String
  , fieldValue :: Maybe Pair
  , helpUrl :: String
  , inputsInline :: Maybe Boolean
  , nextStatement :: Maybe String
  , output :: Maybe String
  , previousStatement :: Maybe String
  , tooltip :: Maybe String
  , extensions :: Array String
  , mutator :: Maybe String
  | r
  )

newtype BlockDefinition
  = BlockDefinition (Record (BasicBlockDefinition (type :: String)))

derive instance newtypeBlockDefinition :: Newtype BlockDefinition _

instance writeForeignBlockDefinition :: WriteForeign BlockDefinition where
  writeImpl (BlockDefinition fields) = JSON.write fields

defaultBlockDefinition ::
  { extensions :: Array String
  , lastDummyAlign0 :: AlignDirection
  , args0 :: Array Arg
  , fieldValue :: Maybe Pair
  , helpUrl :: String
  , inputsInline :: Maybe Boolean
  , mutator :: Maybe String
  , nextStatement :: Maybe String
  , output :: Maybe String
  , previousStatement :: Maybe String
  , tooltip :: Maybe String
  }
defaultBlockDefinition =
  { fieldValue: Nothing
  , lastDummyAlign0: Left
  , args0: []
  , helpUrl: ""
  , inputsInline: Just true
  , nextStatement: Nothing
  , output: Nothing
  , previousStatement: Nothing
  , tooltip: Nothing
  , extensions: []
  , mutator: Nothing
  }

xml :: forall p i. Node (id :: String, style :: String) p i
xml props children = element (ElemName "xml") props children

category :: forall p i. Node (name :: String) p i
category props children = element (ElemName "category") props children

block :: forall p i. Node (id :: String, type :: String, x :: String, y :: String) p i
block props children = element (ElemName "block") props children

blockType :: forall i r. String -> IProp (type :: String | r) i
blockType = attr (AttrName "type")

style :: forall i r. String -> IProp (style :: String | r) i
style = attr (AttrName "style")

name :: forall i r. String -> IProp (name :: String | r) i
name = attr (AttrName "name")

x :: forall i r. String -> IProp (x :: String | r) i
x = attr (AttrName "x")

y :: forall i r. String -> IProp (y :: String | r) i
y = attr (AttrName "y")
