module Marlowe.Blockly where

import Prelude

import Blockly (AlignDirection(..), Arg(..), BlockDefinition(..), block, blockType, category, colour, defaultBlockDefinition, getBlockById, initializeWorkspace, name, render, style, x, xml, y)
import Blockly.Generator (Generator, Input, clearWorkspace, compose2, connectToOutput, connectToPrevious, fieldName, fieldRow, getFieldValue, getInputWithName, inputList, inputName, insertGeneratorFunction, mkGenerator, setFieldText, statementToCode)
import Blockly.Types (Block, BlocklyState, Workspace)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Halogen.HTML (HTML)
import Halogen.HTML.Properties (id_)
import Marlowe.Parser as Parser
import Marlowe.Types (Contract(..), IdChoice(..), Observation(..), Value(..))
import Record (merge)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Basic (parens)

data BlockType
  = BaseContractType
  -- contracts
  | NullContractType
  | CommitContractType
  | PayContractType
  | BothContractType
  | ChoiceContractType
  | WhenContractType
  | WhileContractType
  | ScaleContractType
  | LetContractType
  | UseContractType
  -- observations
  | BelowTimeoutObservationType
  | AndObservationType
  | OrObservationType
  | NotObservationType
  | ChoseThisObservationType
  | ChoseObservationType
  | ValueGEObservationType
  | ValueGObservationType
  | ValueLEObservationType
  | ValueLObservationType
  | ValueEqObservationType
  | TrueObservationType
  | FalseObservationType
  -- values
  | CurrentBlockType
  | CommittedValueType
  | ConstValueType
  | NegValueType
  | AddValueType
  | SubValueType
  | MulValueType
  | DivValueType
  | ModValueType
  | FromChoiceValueType
  | FromOracleValueType

instance showBlockType :: Show BlockType where
  show BaseContractType = "contract"
  -- contracts
  show NullContractType = "contract_null"
  show CommitContractType = "contract_commit"
  show PayContractType = "contract_pay"
  show BothContractType = "contract_both"
  show ChoiceContractType = "contract_choice"
  show WhenContractType = "contract_when"
  show WhileContractType = "contract_while"
  show ScaleContractType = "contract_scale"
  show LetContractType = "contract_let"
  show UseContractType = "contract_use"
  -- observations
  show BelowTimeoutObservationType = "observation_below_timeout"
  show AndObservationType = "observation_and"
  show OrObservationType = "observation_or"
  show NotObservationType = "observation_not"
  show ChoseThisObservationType = "observation_chose_this"
  show ChoseObservationType = "observation_chose"
  show ValueGEObservationType = "observation_ge"
  show ValueGObservationType = "observation_g"
  show ValueLEObservationType = "observation_le"
  show ValueLObservationType = "observation_l"
  show ValueEqObservationType = "observation_eq"
  show TrueObservationType = "observation_true"
  show FalseObservationType = "observation_false"
  -- values
  show CurrentBlockType = "value_current_block"
  show CommittedValueType = "value_committed"
  show ConstValueType = "value_const"
  show NegValueType = "value_neg"
  show AddValueType = "value_add"
  show SubValueType = "value_sub"
  show MulValueType = "value_mul"
  show DivValueType = "value_div"
  show ModValueType = "value_mod"
  show FromChoiceValueType = "value_from_choice"
  show FromOracleValueType = "value_form_oracle"

blockDefinitions :: Array BlockDefinition
blockDefinitions =
  [ baseContract -- this is a container for the entire contract and always exists
  -- contracts
  , nullContract
  , commitContract
  , payContract
  , bothContract
  , choiceContract
  , whenContract
  , whileContract
  , scaleContract
  , letContract
  , useContract
  -- observations
  , belowTimeoutObservation
  , andObservation
  , orObservation
  , notObservation
  , choseThisObservation
  , choseObservation
  , valueGEObservation
  , valueGObservation
  , valueLEObservation
  , valueLObservation
  , valueEqObservation
  , trueObservation
  , falseObservation
  -- values
  , currentBlockValue
  , committedValue
  , constValue
  , negValue
  , addValue
  , subValue
  , mulValue
  , divValue
  , modValue
  , fromChoiceValue
  , fromOracleValue
  ]

baseContract :: BlockDefinition
baseContract =
  BlockDefinition
    $ merge
        { type: show BaseContractType
        , message0: "%1 CONTRACT %2 %3"
        , args0:
          [ DummyRight
          , Statement {name: "contract", check: "contract", align: Right}
          , DummyRight
          ]
        , colour: "0"
        , inputsInline: Just false
        } defaultBlockDefinition

nullContract :: BlockDefinition
nullContract =
  BlockDefinition
    $ merge
        { type: show NullContractType
        , message0: "Null"
        , colour: "0"
        , previousStatement: Just "contract"
        } defaultBlockDefinition

commitContract :: BlockDefinition
commitContract =
  BlockDefinition
    $ merge
        { type: show CommitContractType
        , message0: "Commit %1 with action id %2 %3 and commit id %4 %5 person with id %6 %7 may deposit %8 redeemable on block %9 %10 or after, if money is committed before block %11 %12 continue as %13 otherwise continue as %14"
        , args0:
          [ DummyCentre
          , Number {name: "action_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Number {name: "commit_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Number {name: "person_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Value {name: "ammount", check: "value", align: Right }
          , Number {name: "end_expiration", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Number {name: "start_expiration", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Statement {name: "contract1", check: "contract", align: Right}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        , inputsInline: Just false
        } defaultBlockDefinition

payContract :: BlockDefinition
payContract =
  BlockDefinition
    $ merge
        { type: show PayContractType
        , message0: "Pay %1 with id %2 %3 use money from commit %4 %5 to pay person %6 %7 the amount of %8 if claimed before block %9 continue as %10 else continue as %11"
        , args0:
          [ DummyCentre
          , Number {name: "action_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Number {name: "commit_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Number {name: "payee_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , DummyRight
          , Value {name: "ammount", check: "value", align: Right}
          , Number {name: "timeout", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , Statement {name: "contract1", check: "contract", align: Right}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        , inputsInline: Just false
        } defaultBlockDefinition

bothContract :: BlockDefinition
bothContract =
  BlockDefinition
    $ merge
        { type: show BothContractType
        , message0: "Both %1 enforce both %2 and %3"
        , args0:
          [ DummyCentre
          , Statement {name: "contract1", check: "contract", align: Right}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        } defaultBlockDefinition

choiceContract :: BlockDefinition
choiceContract =
  BlockDefinition
    $ merge
        { type: show ChoiceContractType
        , message0: "Choice %1 if observation %2 then continue as %3 otherwise continue as %4"
        , args0:
          [ DummyCentre
          , Value {name: "observation", check: "observation", align: Right}
          , Statement {name: "contract1", check: "contract", align: Right}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        , inputsInline: Just false
        } defaultBlockDefinition

whenContract :: BlockDefinition
whenContract =
  BlockDefinition
    $ merge
        { type: show WhenContractType
        , message0: "When observation %1 continue as %2 if block is %3 or higher continue as %4"
        , args0:
          [ Value {name: "observation", check: "observation", align: Right}
          , Statement {name: "contract1", check: "contract", align: Right}
          , Number {name: "timeout", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        , inputsInline: Just false
        } defaultBlockDefinition

whileContract :: BlockDefinition
whileContract =
  BlockDefinition
    $ merge
        { type: show WhileContractType
        , message0: "While observation %1 continue as %2 if block is %3 or higher continue as %4"
        , args0:
          [ Value {name: "observation", check: "observation", align: Right}
          , Statement {name: "contract1", check: "contract", align: Right}
          , Number {name: "timeout", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        , inputsInline: Just false
        } defaultBlockDefinition

scaleContract :: BlockDefinition
scaleContract =
  BlockDefinition
    $ merge
        { type: show ScaleContractType
        , message0: "Scale %1 %2 %3 with %4"
        , args0:
          [ Value {name: "scale1", check: "value", align: Right}
          , Value {name: "scale2", check: "value", align: Right}
          , Value {name: "scale3", check: "value", align: Right}
          , Statement {name: "contract", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        } defaultBlockDefinition

letContract :: BlockDefinition
letContract =
  BlockDefinition
    $ merge
        { type: show LetContractType
        , message0: "Let %1 be %2 continue as %3"
        , args0:
          [ Number {name: "let_label", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          , Statement {name: "contract1", check: "contract", align: Right}
          , Statement {name: "contract2", check: "contract", align: Right}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        } defaultBlockDefinition

useContract :: BlockDefinition
useContract =
  BlockDefinition
    $ merge
        { type: show UseContractType
        , message0: "Use %1"
        , args0:
          [ Number {name: "let_label", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          ]
        , colour: "0"
        , previousStatement: Just "contract"
        } defaultBlockDefinition

belowTimeoutObservation :: BlockDefinition
belowTimeoutObservation =
  BlockDefinition
    $ merge
        { type: show BelowTimeoutObservationType
        , message0: "Below Timeout %1"
        , args0:
          [ Number {name: "timeout", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

andObservation :: BlockDefinition
andObservation =
  BlockDefinition
    $ merge
        { type: show AndObservationType
        , message0: "%1 and %2"
        , args0:
          [ Value {name: "observation1", check: "observation", align: Right}
          , Value {name: "observation2", check: "observation", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

orObservation :: BlockDefinition
orObservation =
  BlockDefinition
    $ merge
        { type: show OrObservationType
        , message0: "%1 or %2"
        , args0:
          [ Value {name: "observation1", check: "observation", align: Right}
          , Value {name: "observation2", check: "observation", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

notObservation :: BlockDefinition
notObservation =
  BlockDefinition
    $ merge
        { type: show NotObservationType
        , message0: "Not %1"
        , args0:
          [ Value {name: "observation", check: "observation", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

choseThisObservation :: BlockDefinition
choseThisObservation =
  BlockDefinition
    $ merge
        { type: show ChoseThisObservationType
        , message0: "chose id %1 for person %2 choice %3"
        , args0:
          [ Number {name: "choice_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Number {name: "person_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Number {name: "choice", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

choseObservation :: BlockDefinition
choseObservation =
  BlockDefinition
    $ merge
        { type: show ChoseObservationType
        , message0: "chose id %1 for person %2"
        , args0:
          [ Number {name: "choice_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Number {name: "person_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

valueGEObservation :: BlockDefinition
valueGEObservation =
  BlockDefinition
    $ merge
        { type: show ValueGEObservationType
        , message0: "value %1 is greater than or equal to %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

valueGObservation :: BlockDefinition
valueGObservation =
  BlockDefinition
    $ merge
        { type: show ValueGObservationType
        , message0: "value %1 is greater than %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

valueLEObservation :: BlockDefinition
valueLEObservation =
  BlockDefinition
    $ merge
        { type: show ValueLEObservationType
        , message0: "value %1 is less than or equal to %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

valueLObservation :: BlockDefinition
valueLObservation =
  BlockDefinition
    $ merge
        { type: show ValueLObservationType
        , message0: "value %1 is less than %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

valueEqObservation :: BlockDefinition
valueEqObservation =
  BlockDefinition
    $ merge
        { type: show ValueEqObservationType
        , message0: "value %1 is equal to %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

trueObservation :: BlockDefinition
trueObservation =
  BlockDefinition
    $ merge
        { type: show TrueObservationType
        , message0: "true"
        , lastDummyAlign0: Centre
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

falseObservation :: BlockDefinition
falseObservation =
  BlockDefinition
    $ merge
        { type: show FalseObservationType
        , message0: "false"
        , lastDummyAlign0: Centre
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        } defaultBlockDefinition

currentBlockValue :: BlockDefinition
currentBlockValue =
  BlockDefinition
    $ merge
        { type: show CurrentBlockType
        , message0: "Current Block"
        , lastDummyAlign0: Centre
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

committedValue :: BlockDefinition
committedValue =
  BlockDefinition
    $ merge
        { type: show CommittedValueType
        , message0: "Committed Value %1"
        , args0:
          [ Number {name: "commit_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

constValue :: BlockDefinition
constValue =
  BlockDefinition
    $ merge
        { type: show ConstValueType
        , message0: "Constant Value %1 ADA"
        , args0:
          [ Number {name: "constant", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

negValue :: BlockDefinition
negValue =
  BlockDefinition
    $ merge
        { type: show NegValueType
        , message0: "Negate Value %1"
        , args0:
          [ Value {name: "value", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

addValue :: BlockDefinition
addValue =
  BlockDefinition
    $ merge
        { type: show AddValueType
        , message0: "%1 + %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

subValue :: BlockDefinition
subValue =
  BlockDefinition
    $ merge
        { type: show SubValueType
        , message0: "%1 - %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

mulValue :: BlockDefinition
mulValue =
  BlockDefinition
    $ merge
        { type: show MulValueType
        , message0: "%1 * %2"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

divValue :: BlockDefinition
divValue =
  BlockDefinition
    $ merge
        { type: show DivValueType
        , message0: "%1 / %2 with default %3"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          , Value {name: "value3", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

modValue :: BlockDefinition
modValue =
  BlockDefinition
    $ merge
        { type: show ModValueType
        , message0: "%1 % %2 with default %3"
        , args0:
          [ Value {name: "value1", check: "value", align: Right}
          , Value {name: "value2", check: "value", align: Right}
          , Value {name: "value3", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

fromChoiceValue :: BlockDefinition
fromChoiceValue =
  BlockDefinition
    $ merge
        { type: show FromChoiceValueType
        , message0: "use value of choice with id: %1 chosen by participant with id: %2 if no choice was made use: %3"
        , args0:
          [ Number {name: "choice_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Number {name: "person_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Value {name: "value", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

fromOracleValue :: BlockDefinition
fromOracleValue =
  BlockDefinition
    $ merge
        { type: show FromOracleValueType
        , message0: "use value of oracle with id: %1 if no oracle exists use: %2"
        , args0:
          [ Number {name: "oracle_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing}
          , Value {name: "value", check: "value", align: Right}
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        } defaultBlockDefinition

toolbox :: forall a b. HTML a b
toolbox =
  xml [id_ "blocklyToolbox", style "display:none"]
    [ category [name "Contracts", colour "0"]
        [ block [blockType (unwrap nullContract).type] []
        , block [blockType (unwrap commitContract).type] []
        , block [blockType (unwrap payContract).type] []
        , block [blockType (unwrap bothContract).type] []
        , block [blockType (unwrap choiceContract).type] []
        , block [blockType (unwrap whenContract).type] []
        , block [blockType (unwrap whileContract).type] []
        , block [blockType (unwrap scaleContract).type] []
        , block [blockType (unwrap letContract).type] []
        , block [blockType (unwrap useContract).type] []
        ]
    , category [name "Observations", colour "230"]
        [ block [blockType (unwrap belowTimeoutObservation).type] []
        , block [blockType (unwrap andObservation).type] []
        , block [blockType (unwrap orObservation).type] []
        , block [blockType (unwrap notObservation).type] []
        , block [blockType (unwrap choseThisObservation).type] []
        , block [blockType (unwrap choseObservation).type] []
        , block [blockType (unwrap valueGEObservation).type] []
        , block [blockType (unwrap valueGObservation).type] []
        , block [blockType (unwrap valueLEObservation).type] []
        , block [blockType (unwrap valueLObservation).type] []
        , block [blockType (unwrap valueEqObservation).type] []
        , block [blockType (unwrap trueObservation).type] []
        , block [blockType (unwrap falseObservation).type] []
        ]
    , category [name "Values", colour "135"]
        [ block [blockType (unwrap currentBlockValue).type] []
        , block [blockType (unwrap committedValue).type] []
        , block [blockType (unwrap constValue).type] []
        , block [blockType (unwrap negValue).type] []
        , block [blockType (unwrap addValue).type] []
        , block [blockType (unwrap subValue).type] []
        , block [blockType (unwrap mulValue).type] []
        , block [blockType (unwrap divValue).type] []
        , block [blockType (unwrap modValue).type] []
        , block [blockType (unwrap fromChoiceValue).type] []
        , block [blockType (unwrap fromOracleValue).type] []
        ]
    ]

workspaceBlocks :: forall a b. HTML a b
workspaceBlocks =
  xml [id_ "workspaceBlocks", style "display:none"]
    [ block [blockType (unwrap baseContract).type, x "13", y "187", id_ "root_contract"] []
    ]

parse :: forall a. Parser String a -> String -> Either String a
parse p = lmap show <<< flip runParser (parens p <|> p)

buildGenerator :: BlocklyState -> Effect Generator
buildGenerator blocklyState = do
  g <- mkGenerator blocklyState "Marlowe"
  mkGenFun g BaseContractType
    $ \bs block -> parse Parser.contract =<< statementToCode g block (show BaseContractType)
  -- contracts
  mkGenFun g NullContractType $ \_ _ -> pure Null
  mkGenFun g CommitContractType
    $ \bs block -> do
        actionId <- parse Parser.idAction =<< getFieldValue block "action_id"
        commitId <- parse Parser.idCommit =<< getFieldValue block "commit_id"
        personId <- parse Parser.person =<< getFieldValue block "person_id"
        ammount <- parse Parser.value =<< statementToCode g block "ammount"
        endExpiration <- parse Parser.timeout =<< getFieldValue block "end_expiration"
        startExpiration <- parse Parser.timeout =<< getFieldValue block "start_expiration"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (Commit actionId commitId personId ammount endExpiration startExpiration contract1 contract2)
  mkGenFun g PayContractType
    $ \bs block -> do
        actionId <- parse Parser.idAction =<< getFieldValue block "action_id"
        commitId <- parse Parser.idCommit =<< getFieldValue block "commit_id"
        payee <- parse Parser.person =<< getFieldValue block "payee_id"
        ammount <- parse Parser.value =<< statementToCode g block "ammount"
        timeout <- parse Parser.timeout =<< getFieldValue block "timeout"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (Pay actionId commitId payee ammount timeout contract1 contract2)
  mkGenFun g BothContractType
    $ \bs block -> do
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (Both contract1 contract2)
  mkGenFun g ChoiceContractType
    $ \bs block -> do
        observation <- parse Parser.observation =<< statementToCode g block "observation"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (Choice observation contract1 contract2)
  mkGenFun g WhenContractType
    $ \bs block -> do
        observation <- parse Parser.observation =<< statementToCode g block "observation"
        timeout <- parse Parser.timeout =<< getFieldValue block "timeout"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (When observation timeout contract1 contract2)
  mkGenFun g WhileContractType
    $ \bs block -> do
        observation <- parse Parser.observation =<< statementToCode g block "observation"
        timeout <- parse Parser.timeout =<< getFieldValue block "timeout"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (While observation timeout contract1 contract2)
  mkGenFun g ScaleContractType
    $ \bs block -> do
        scale1 <- parse Parser.value =<< statementToCode g block "scale1"
        scale2 <- parse Parser.value =<< statementToCode g block "scale2"
        scale3 <- parse Parser.value =<< statementToCode g block "scale3"
        contract <- parse Parser.contract =<< statementToCode g block "contract"
        pure (Scale scale1 scale2 scale3 contract)
  mkGenFun g LetContractType
    $ \bs block -> do
        letLabel <- parse Parser.bigInteger =<< getFieldValue block "let_label"
        contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
        contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
        pure (Let letLabel contract1 contract2)
  mkGenFun g UseContractType
    $ \bs block -> do
        letLabel <- parse Parser.bigInteger =<< getFieldValue block "let_label"
        pure (Use letLabel)
  -- observations
  mkGenFun g BelowTimeoutObservationType
    $ \bs block -> do
        timeout <- parse Parser.timeout =<< getFieldValue block "timeout"
        pure (BelowTimeout timeout)
  mkGenFun g AndObservationType
    $ \bs block -> do
        observation1 <- parse Parser.observation =<< statementToCode g block "observation1"
        observation2 <- parse Parser.observation =<< statementToCode g block "observation2"
        pure (AndObs observation1 observation2)
  mkGenFun g OrObservationType
    $ \bs block -> do
        observation1 <- parse Parser.observation =<< statementToCode g block "observation1"
        observation2 <- parse Parser.observation =<< statementToCode g block "observation2"
        pure (OrObs observation1 observation2)
  mkGenFun g NotObservationType
    $ \bs block -> do
        observation <- parse Parser.observation =<< statementToCode g block "observation"
        pure (NotObs observation)
  mkGenFun g ChoseThisObservationType
    $ \bs block -> do
        choiceId <- parse Parser.bigInteger =<< getFieldValue block "choice_id"
        personId <- parse Parser.person =<< getFieldValue block "person_id"
        choice <- parse Parser.choice =<< getFieldValue block "choice"
        let
          idChoice = IdChoice {choice: choiceId, person: personId}
        pure (ChoseThis idChoice choice)
  mkGenFun g ChoseObservationType
    $ \bs block -> do
        choiceId <- parse Parser.bigInteger =<< getFieldValue block "choice_id"
        personId <- parse Parser.person =<< getFieldValue block "person_id"
        let
          idChoice = IdChoice {choice: choiceId, person: personId}
        pure (ChoseSomething idChoice)
  mkGenFun g ValueGEObservationType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (ValueGE value1 value2)
  mkGenFun g ValueGObservationType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (ValueGT value1 value2)
  mkGenFun g ValueLEObservationType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (ValueLE value1 value2)
  mkGenFun g ValueLObservationType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (ValueLT value1 value2)
  mkGenFun g ValueEqObservationType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (ValueEQ value1 value2)
  mkGenFun g TrueObservationType $ \bs block -> pure TrueObs
  mkGenFun g FalseObservationType $ \bs block -> pure FalseObs
  -- values
  mkGenFun g CurrentBlockType $ \bs block -> pure CurrentBlock
  mkGenFun g CommittedValueType
    $ \bs block -> do
        commitId <- parse Parser.idCommit =<< getFieldValue block "commit_id"
        pure (Committed commitId)
  mkGenFun g ConstValueType
    $ \bs block -> do
        constant <- parse Parser.bigInteger =<< getFieldValue block "constant"
        pure (Constant constant)
  mkGenFun g NegValueType
    $ \bs block -> do
        value <- parse Parser.value =<< statementToCode g block "value"
        pure (NegValue value)
  mkGenFun g AddValueType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (AddValue value1 value2)
  mkGenFun g SubValueType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (SubValue value1 value2)
  mkGenFun g MulValueType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        pure (MulValue value1 value2)
  mkGenFun g DivValueType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        value3 <- parse Parser.value =<< statementToCode g block "value3"
        pure (DivValue value1 value2 value3)
  mkGenFun g ModValueType
    $ \bs block -> do
        value1 <- parse Parser.value =<< statementToCode g block "value1"
        value2 <- parse Parser.value =<< statementToCode g block "value2"
        value3 <- parse Parser.value =<< statementToCode g block "value3"
        pure (ModValue value1 value2 value3)
  mkGenFun g FromChoiceValueType
    $ \bs block -> do
        choiceId <- parse Parser.bigInteger =<< getFieldValue block "choice_id"
        personId <- parse Parser.person =<< getFieldValue block "person_id"
        value <- parse Parser.value =<< statementToCode g block "value"
        let
          idChoice = IdChoice {choice: choiceId, person: personId}
        pure (ValueFromChoice idChoice value)
  mkGenFun g FromOracleValueType
    $ \bs block -> do
        oracleId <- parse Parser.idOracle =<< getFieldValue block "oracle_id"
        value <- parse Parser.value =<< statementToCode g block "value"
        pure (ValueFromOracle oracleId value)
  pure g
  where
  mkGenFun :: forall a. Show a => Generator -> BlockType -> (BlocklyState -> Block -> Either String a) -> Effect Unit
  mkGenFun g t f = insertGeneratorFunction blocklyState g (show t) (compose2 (rmap show) f)

buildBlocks :: (Workspace -> String -> Effect Block) -> BlocklyState -> Contract -> Effect Unit
buildBlocks newBlock bs contract = do
  clearWorkspace bs.workspace
  initializeWorkspace bs
  let
    mContract = getBlockById bs.workspace "root_contract"
  rootBlock <- case mContract of
    Nothing -> newBlock bs.workspace "contract"
    Just block -> pure block
  let
    inputs = inputList rootBlock

    mInput = getInputWithName inputs "contract"
  case mInput of
    Nothing -> pure unit
    Just i -> do
      toBlockly newBlock bs.workspace i contract
      render bs.workspace

setField :: Block -> String -> String -> Effect Unit
setField block name value =
  let
    fields = inputList block >>= fieldRow
  in
    case Array.find (\f -> fieldName f == name) fields of
      Nothing -> pure unit
      Just f -> setFieldText f value

inputToBlockly :: forall a. ToBlockly a => (Workspace -> String -> Effect Block) -> Workspace -> Block -> String -> a -> Effect Unit
inputToBlockly newBlock workspace block name value = case Array.find (\i -> inputName i == name) (inputList block) of
  Nothing -> pure unit
  Just input -> toBlockly newBlock workspace input value

class ToBlockly a where
  toBlockly :: (Workspace -> String -> Effect Block) -> Workspace -> Input -> a -> Effect Unit

instance toBlocklyContract :: ToBlockly Contract where
  toBlockly newBlock workspace input Null = do
    block <- newBlock workspace (show NullContractType)
    connectToPrevious block input
  toBlockly newBlock workspace input (Commit action commit person ammount endExpiration startExpiration contract1 contract2) = do
    block <- newBlock workspace (show CommitContractType)
    connectToPrevious block input
    setField block "action_id" (show (unwrap action))
    setField block "commit_id" (show (unwrap commit))
    setField block "person_id" (show person)
    inputToBlockly newBlock workspace block "ammount" ammount
    setField block "end_expiration" (show (unwrap endExpiration))
    setField block "start_expiration" (show (unwrap startExpiration))
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (Pay action commit payee ammount timeout contract1 contract2) = do
    block <- newBlock workspace (show PayContractType)
    connectToPrevious block input
    setField block "action_id" (show (unwrap action))
    setField block "commit_id" (show (unwrap commit))
    setField block "payee_id" (show payee)
    inputToBlockly newBlock workspace block "ammount" ammount
    setField block "timeout" (show timeout)
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (Both contract1 contract2) = do
    block <- newBlock workspace (show BothContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (Choice observation contract1 contract2) = do
    block <- newBlock workspace (show ChoiceContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "observation" observation
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (When observation timeout contract1 contract2) = do
    block <- newBlock workspace (show WhenContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "observation" observation
    setField block "timeout" (show timeout)
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (While observation timeout contract1 contract2) = do
    block <- newBlock workspace (show WhileContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "observation" observation
    setField block "timeout" (show timeout)
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (Scale v1 v2 v3 contract) = do
    block <- newBlock workspace (show ScaleContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "scale1" v1
    inputToBlockly newBlock workspace block "scale2" v2
    inputToBlockly newBlock workspace block "scale3" v3
    inputToBlockly newBlock workspace block "contract" contract
  toBlockly newBlock workspace input (Let label contract1 contract2) = do
    block <- newBlock workspace (show LetContractType)
    connectToPrevious block input
    setField block "let_label" (show label)
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (Use label) = do
    block <- newBlock workspace (show UseContractType)
    connectToPrevious block input
    setField block "let_label" (show label)

instance toBlocklyObservation :: ToBlockly Observation where
  toBlockly newBlock workspace input (BelowTimeout timeout) = do
    block <- newBlock workspace (show BelowTimeoutObservationType)
    connectToOutput block input
    setField block "timeout" (show timeout)
  toBlockly newBlock workspace input (AndObs observation1 observation2) = do
    block <- newBlock workspace (show AndObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation1" observation1
    inputToBlockly newBlock workspace block "observation2" observation2
  toBlockly newBlock workspace input (OrObs observation1 observation2) = do
    block <- newBlock workspace (show OrObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation1" observation1
    inputToBlockly newBlock workspace block "observation2" observation2
  toBlockly newBlock workspace input (NotObs observation) = do
    block <- newBlock workspace (show NotObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation" observation
  toBlockly newBlock workspace input (ChoseThis choiceId choice) = do
    block <- newBlock workspace (show ChoseThisObservationType)
    connectToOutput block input
    setField block "choice_id" (show (unwrap choiceId).choice)
    setField block "person_id" (show (unwrap choiceId).person)
    setField block "choice" (show choice)
  toBlockly newBlock workspace input (ChoseSomething choiceId) = do
    block <- newBlock workspace (show ChoseObservationType)
    connectToOutput block input
    setField block "choice_id" (show (unwrap choiceId).choice)
    setField block "person_id" (show (unwrap choiceId).person)
  toBlockly newBlock workspace input (ValueGE v1 v2) = do
    block <- newBlock workspace (show ValueGEObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueGT v1 v2) = do
    block <- newBlock workspace (show ValueGObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueLT v1 v2) = do
    block <- newBlock workspace (show ValueLObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueLE v1 v2) = do
    block <- newBlock workspace (show ValueLEObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueEQ v1 v2) = do
    block <- newBlock workspace (show ValueEqObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input TrueObs = do
    block <- newBlock workspace (show TrueObservationType)
    connectToOutput block input
  toBlockly newBlock workspace input FalseObs = do
    block <- newBlock workspace (show FalseObservationType)
    connectToOutput block input

instance toBlocklyValue :: ToBlockly Value where
  toBlockly newBlock workspace input CurrentBlock = do
    block <- newBlock workspace (show CurrentBlockType)
    connectToOutput block input
  toBlockly newBlock workspace input (Committed v) = do
    block <- newBlock workspace (show CommittedValueType)
    connectToOutput block input
    setField block "commit_id" (show v)
  toBlockly newBlock workspace input (Constant v) = do
    block <- newBlock workspace (show ConstValueType)
    connectToOutput block input
    setField block "constant" (show v)
  toBlockly newBlock workspace input (NegValue v) = do
    block <- newBlock workspace (show NegValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value" v
  toBlockly newBlock workspace input (AddValue v1 v2) = do
    block <- newBlock workspace (show AddValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (SubValue v1 v2) = do
    block <- newBlock workspace (show SubValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (MulValue v1 v2) = do
    block <- newBlock workspace (show MulValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (DivValue v1 v2 v3) = do
    block <- newBlock workspace (show DivValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
    inputToBlockly newBlock workspace block "value3" v3
  toBlockly newBlock workspace input (ModValue v1 v2 v3) = do
    block <- newBlock workspace (show ModValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
    inputToBlockly newBlock workspace block "value3" v3
  toBlockly newBlock workspace input (ValueFromChoice v1 v2) = do
    block <- newBlock workspace (show FromChoiceValueType)
    connectToOutput block input
    setField block "choice_id" (show (unwrap v1).choice)
    setField block "person_id" (show (unwrap v1).person)
    inputToBlockly newBlock workspace block "value" v2
  toBlockly newBlock workspace input (ValueFromOracle v1 v2) = do
    block <- newBlock workspace (show FromOracleValueType)
    connectToOutput block input
    setField block "oracle_id" (show (unwrap v1))
    inputToBlockly newBlock workspace block "value" v2
