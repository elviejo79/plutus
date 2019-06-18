module Halogen.Blockly where

import Blockly (BlockDefinition, Div(..))
import Blockly as Blockly
import Blockly.Generator (Generator, newBlock, workspaceToCode)
import Blockly.Types as BT
import Bootstrap (btn, btnInfo, btnSmall)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (ClassName(..), Component, ComponentDSL, ComponentHTML, RefLabel(..), action, get, lifecycleComponent, liftEffect, modify, raise)
import Halogen.HTML (button, div, text)
import Halogen.HTML as HH
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, id_, ref)
import Marlowe.Blockly (buildBlocks, buildGenerator)
import Marlowe.Parser as Parser
import Marlowe.Pretty (pretty)
import Marlowe.Types (Contract(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Basic (parens)

type BlocklyState
  = {blocklyState :: Maybe BT.BlocklyState, generator :: Maybe Generator}

data BlocklyQuery a
  = Inject (Array BlockDefinition) a
  | Resize a
  | SetData Unit a
  | GetCode a
  | SetCode String a

data BlocklyMessage
  = Initialized
  | CurrentCode String

type HTML
  = ComponentHTML BlocklyQuery

type DSL m
  = ComponentDSL BlocklyState BlocklyQuery BlocklyMessage m

blockly :: forall m. MonadEffect m => Array BlockDefinition -> Component HH.HTML BlocklyQuery Unit BlocklyMessage m
blockly blockDefinitions =
  lifecycleComponent
    { initialState: \_ -> {blocklyState: Nothing, generator: Nothing}
    , render
    , eval
    , initializer: Just $ action (Inject blockDefinitions)
    , finalizer: Nothing
    , receiver: HE.input SetData
    }

eval :: forall m. MonadEffect m => BlocklyQuery ~> DSL m
eval (Inject blockDefinitions next) = do
  blocklyState <- liftEffect $ Blockly.createBlocklyInstance (Div "blocklyWorkspace") (Div "blocklyToolbox")
  liftEffect $ Blockly.addBlockTypes blocklyState blockDefinitions
  liftEffect $ Blockly.initializeWorkspace blocklyState
  generator <- liftEffect $ buildGenerator blocklyState
  _ <- modify _ {blocklyState = Just blocklyState, generator = Just generator}
  pure next

eval (Resize next) = do
  state <- get
  _ <- liftEffect $ Blockly.resize state.blocklyState
  pure next

eval (SetData _ next) = pure next

eval (GetCode next) = do
  {blocklyState, generator} <- get
  case blocklyState of
    Nothing -> pure unit
    Just bs -> case generator of
      Nothing -> pure unit
      Just g -> do
        code <- liftEffect $ workspaceToCode bs g
        let
          result = runParser code (parens Parser.contract <|> Parser.contract)
        case result of
          Left e -> do
            log (show e)
            pure unit
          Right contract -> raise $ CurrentCode (show (pretty contract))
  pure next

eval (SetCode code next) = do
  {blocklyState} <- get
  let
    contract = case runParser code Parser.contract of
      Right c -> c
      Left _ -> Null
  case blocklyState of
    Nothing -> pure unit
    Just bs -> do
      liftEffect $ buildBlocks newBlock bs contract
      pure unit
  pure next

blocklyRef :: RefLabel
blocklyRef = RefLabel "blockly"

render :: BlocklyState -> HTML
render state =
  div []
    [ div
        [ ref blocklyRef
        , id_ "blocklyWorkspace"
        , classes [ClassName "blockly-workspace", ClassName "container-fluid"]
        ] [toCodeButton "To Code"]
    ]

toCodeButton :: String -> HTML
toCodeButton key =
  button
    [ classes [btn, btnInfo, btnSmall]
    , onClick $ input_ GetCode
    ] [text key]
