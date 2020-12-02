module internal QsFmt.Formatter.SyntaxTree.Type

open QsFmt.Formatter.SyntaxTree.Node

type CharacteristicGroup =
    { OpenParen: Terminal
      Characteristic: Characteristic
      CloseParen: Terminal }

and CharacteristicBinaryOperator =
    { Left: Characteristic
      Operator: Terminal
      Right: Characteristic }

and Characteristic =
    | Adjoint
    | Controlled
    | CharacteristicGroup of CharacteristicGroup
    | CharacteristicBinaryOperator of CharacteristicBinaryOperator

type CharacteristicSection =
    { IsKeyword: Terminal
      Characteristic: Characteristic }

type TupleType =
    { OpenParen: Terminal
      Items: Type SequenceItem list
      CloseParen: Terminal }

and ArrayType =
    { BaseType: Type
      OpenBracket: Terminal
      CloseBracket: Terminal }

and CallableType =
    { OpenParen: Terminal
      InnerOpenParen: Terminal
      FromType: Type
      Arrow: Terminal
      ToType: Type
      InnerCloseParen: Terminal
      Characteristics: CharacteristicSection option
      CloseParen: Terminal }

and Type =
    | MissingType of Terminal
    | TypeParameter of Terminal
    | BuiltInType of Terminal
    | UserDefinedType of Terminal
    | TupleType of TupleType
    | ArrayType of ArrayType
    | CallableType of CallableType
