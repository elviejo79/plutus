module Blockly.Types where

foreign import data Blockly :: Type

foreign import data Workspace :: Type

foreign import data Block :: Type

type BlocklyState = { blockly :: Blockly, workspace :: Workspace }