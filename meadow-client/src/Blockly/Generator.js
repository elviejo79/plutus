/*eslint-env node*/
'use strict';

exports.getFieldValue_ = function (left, right, block, key) {
    var result = block.getFieldValue(key);
    if (result) {
        return right(result);
    } else {
        return left("couldn't find field: " + key);
    }
}

exports.statementToCode_ = function (left, right, generator, block, key) {
    var result = generator.statementToCode(block, key);
    if (result) {
        // Blockly adds some whitespace for some reason
        return right(result.trim());
    } else {
        return left("couldn't find statement: " + key);
    }
}

exports.valueToCode_ = function (left, right, generator, block, key, order) {
    var result = generator.valueToCode(block, key, order);
    if (result) {
        // Blockly adds some whitespace for some reason
        return right(result.trim());
    } else {
        return left("couldn't find value: " + key);
    }
}

exports.mkGenerator_ = function (blocklyState, name) {
    return function () {
        return new blocklyState.blockly.Generator(name);
    };
}

exports.insertGeneratorFunction_ = function (genRef) {
    return function (key, f) {
        console.log('ran this');
        return function () {
            console.log('didnt run this');
            var generator = genRef.value;
            generator[key] = f;
        };
    };
}

exports.workspaceToCode_ = function (blocklyState, generator) {
    console.log(generator);
    return generator.workspaceToCode(blocklyState.workspace);
}

exports.inputList_ = function (block) {
    return block.inputList;
}

exports.connectToPrevious_ = function (block, input) {
    block.previousConnection.connect(input.connection);
}

exports.connectToOutput_ = function (block, input) {
    block.outputConnection.connect(input.connection);
}

exports.newBlock_ = function (workspace, name) {
    var block = workspace.newBlock(name);
    block.initSvg();
    return block;
}

exports.inputName_ = function (input) {
    return input.name;
}

exports.clearWorkspace_ = function (workspace) {
    workspace.clear()
}

exports.fieldRow_ = function (input) {
    return input.fieldRow;
}

exports.setFieldText_ = function (field, text) {
    field.setText(text);
}

exports.fieldName_ = function (field) {
    return field.name;
}

exports.unsafeThrowError_ = function (s) {
    throw new Error(s);
}