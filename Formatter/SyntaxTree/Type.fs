namespace QsFmt.Formatter.SyntaxTree

type internal CharacteristicGroup =
    { OpenParen: Terminal
      Characteristic: Characteristic
      CloseParen: Terminal }

and internal CharacteristicBinaryOperator =
    { Left: Characteristic
      Operator: Terminal
      Right: Characteristic }

and internal Characteristic =
    | Adjoint of Terminal
    | Controlled of Terminal
    | CharacteristicGroup of CharacteristicGroup
    | CharacteristicBinaryOperator of CharacteristicBinaryOperator

type internal CharacteristicSection =
    { IsKeyword: Terminal
      Characteristic: Characteristic }

and internal ArrayType =
    { BaseType: Type
      OpenBracket: Terminal
      CloseBracket: Terminal }

and internal CallableType =
    { OpenParen: Terminal
      InnerOpenParen: Terminal
      FromType: Type
      Arrow: Terminal
      ToType: Type
      InnerCloseParen: Terminal
      Characteristics: CharacteristicSection option
      CloseParen: Terminal }

and internal Type =
    | MissingType of Terminal
    | TypeParameter of Terminal
    | BuiltInType of Terminal
    | UserDefinedType of Terminal
    | TupleType of Type Tuple
    | ArrayType of ArrayType
    | CallableType of CallableType

type internal TypeAnnotation = { Colon: Terminal; Type: Type }
