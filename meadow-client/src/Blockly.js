/*eslint-env node*/
'use strict';

exports.createBlocklyInstance_ = function() {
    return require('node-blockly/browser');
}

exports.createWorkspace_ = function(blockly, workspaceDiv, toolboxDiv) {
    var workspace = blockly.inject(workspaceDiv, {toolbox: document.getElementById(toolboxDiv)
                                                 , collapse : true 
                                                 , comments : true 
                                                 , disable : true
                                                 , maxBlocks : Infinity
                                                 , trashcan : true
                                                 , horizontalLayout : false
                                                 , toolboxPosition : 'start'
                                                 , css : true
                                                 , media : 'https://blockly-demo.appspot.com/static/media/'
                                                 , rtl : false
                                                 , scrollbars : true
                                                 , sounds : true
                                                 , oneBasedIndex : true
                                                 });
    blockly.svgResize(workspace);
    return workspace;

}

exports.resizeBlockly_ = function(blocklyState) {
    var Blockly = blocklyState.blockly;
    var workspace = blocklyState.workspace;
    Blockly.svgResize(workspace);
    workspace.render();
}

function removeUndefinedFields(obj) {
    for (var propName in obj) { 
        if (obj[propName] === undefined) {
          delete obj[propName];
        }
      }
  }

function removeEmptyArrayFields(obj) {
    for (var propName in obj) { 
        if (Array.isArray(obj[propName]) && obj[propName].length == 0) {
          delete obj[propName];
        }
      }
}

exports.addBlockType_ = function(blocklyState, name, block) {
    var Blockly = blocklyState.blockly;
    removeUndefinedFields(block);
    removeEmptyArrayFields(block);
    Blockly.Blocks[name] = {
        init: function() {
            this.jsonInit(block);
        }
    }
}

exports.initializeWorkspace_ = function(blockly, workspace) {
    var workspaceBlocks = document.getElementById("workspaceBlocks");
    blockly.Xml.domToWorkspace(workspaceBlocks, workspace);
    workspace.getAllBlocks()[0].setDeletable(false);
}

exports.render_ = function(workspace) {
    workspace.render();
}

exports.getBlockById_ = function(just, nothing, workspace, id) {
    var result = workspace.getBlockById(id);
    if (result) {
        return just(result);
    } else {
        return nothing;
    }
}