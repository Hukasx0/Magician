module Lib where

type Label = String

data DataType = Text String | Number String | List [DataType]
                | Letter Char | None () | StructUse Label | Function Operation

data BinaryOperation = Plus DataType DataType | Minus DataType DataType

data Operation = Do [Operation] | IfThen BinaryOperation Operation | StructDef [DataType]
                | IsA Label String | IsEq Label DataType | Print DataType | EndS () | EndF ()
