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
    | Group of CharacteristicGroup
    | BinaryOperator of CharacteristicBinaryOperator

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
    | Missing of Terminal
    | Parameter of Terminal
    | BuiltIn of Terminal
    | UserDefined of Terminal
    | Tuple of Type Tuple
    | Array of ArrayType
    | Callable of CallableType
    | Unknown of Terminal

type internal TypeAnnotation = { Colon: Terminal; Type: Type }
