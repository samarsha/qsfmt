module internal QsFmt.Formatter.SyntaxTree.Type

open QsFmt.Formatter.SyntaxTree.Node

type CharacteristicGroup =
    { OpenParen: Terminal Node
      Characteristic: Characteristic
      CloseParen: Terminal Node }

and CharacteristicBinaryOperator =
    { Left: Characteristic
      Operator: Terminal Node
      Right: Characteristic }

and Characteristic =
    | Adjoint
    | Controlled
    | CharacteristicGroup of CharacteristicGroup
    | CharacteristicBinaryOperator of CharacteristicBinaryOperator

type CharacteristicSection =
    { IsKeyword: Terminal Node
      Characteristic: Characteristic }

type TupleType =
    { OpenParen: Terminal Node
      Items: Type SequenceItem list
      CloseParen: Terminal Node }

and ArrayType =
    { BaseType: Type
      OpenBracket: Terminal Node
      CloseBracket: Terminal Node }

and CallableType =
    { OpenParen: Terminal Node
      InnerOpenParen: Terminal Node
      FromType: Type
      Arrow: Terminal Node
      ToType: Type
      InnerCloseParen: Terminal Node
      Characteristics: CharacteristicSection option
      CloseParen: Terminal Node }

and Type =
    | MissingType of Terminal Node
    | TypeParameter of Terminal Node
    | BuiltInType of Terminal Node
    | UserDefinedType of Terminal Node
    | TupleType of TupleType
    | ArrayType of ArrayType
    | CallableType of CallableType
