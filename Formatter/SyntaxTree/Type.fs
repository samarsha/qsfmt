module internal QsFmt.Formatter.SyntaxTree.Type

open QsFmt.Formatter.SyntaxTree.Node

type CharacteristicGroup =
    { OpenParen : Terminal Node
      Characteristic : Characteristic Node
      CloseParen : Terminal Node }

and CharacteristicBinaryOperator =
    { Left : Characteristic Node
      Operator : Terminal Node
      Right : Characteristic Node }

and Characteristic =
    | Adjoint
    | Controlled
    | CharacteristicGroup of CharacteristicGroup
    | CharacteristicBinaryOperator of CharacteristicBinaryOperator

type CharacteristicSection =
    { IsKeyword : Terminal Node
      Characteristic : Characteristic Node }

type TupleType =
    { OpenParen : Terminal Node
      Items : Type SequenceItem list
      CloseParen : Terminal Node }

and ArrayType =
    { BaseType : Type Node
      OpenBracket : Terminal Node
      CloseBracket : Terminal Node }

and CallableType =
    { OpenParen : Terminal Node
      InnerOpenParen : Terminal Node
      FromType : Type Node
      Arrow : Terminal Node
      ToType : Type Node
      InnerCloseParen : Terminal Node
      Characteristic : CharacteristicSection Node option
      CloseParen : Terminal Node }

and Type =
    | MissingType
    | TypeParameter of string
    | BigInt
    | Bool
    | Double
    | Int
    | Pauli
    | Qubit
    | Range
    | Result
    | String
    | Unit
    | UserDefinedType of string
    | TupleType of TupleType
    | ArrayType of ArrayType
    | CallableType of CallableType
