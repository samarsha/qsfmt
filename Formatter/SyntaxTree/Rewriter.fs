namespace QsFmt.Formatter.SyntaxTree.Rewriter

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type internal 'data Rewriter() =
    abstract Program: 'data -> Program -> Program

    abstract Namespace: 'data -> Namespace -> Namespace

    abstract NamespaceElement: 'data -> NamespaceElement -> NamespaceElement

    abstract CallableDeclaration: 'data -> CallableDeclaration -> CallableDeclaration

    abstract Type: 'data -> Type -> Type

    abstract TupleType: 'data -> TupleType -> TupleType

    abstract ArrayType: 'data -> ArrayType -> ArrayType

    abstract CallableType: 'data -> CallableType -> CallableType

    abstract CharacteristicSection: 'data -> CharacteristicSection -> CharacteristicSection

    abstract CharacteristicGroup: 'data -> CharacteristicGroup -> CharacteristicGroup

    abstract CharacteristicBinaryOperator: 'data -> CharacteristicBinaryOperator -> CharacteristicBinaryOperator

    abstract Characteristic: 'data -> Characteristic -> Characteristic

    abstract Statement: 'data -> Statement -> Statement

    abstract Let: 'data -> Let -> Let

    abstract Return: 'data -> Return -> Return

    abstract SymbolBinding: 'data -> SymbolBinding -> SymbolBinding

    abstract Expression: 'data -> Expression -> Expression

    abstract Tuple: 'data -> Tuple -> Tuple

    abstract BinaryOperator: 'data -> BinaryOperator -> BinaryOperator

    abstract Update: 'data -> Update -> Update

    abstract SequenceItem: 'data -> ('data -> 'a -> 'a) -> 'a SequenceItem -> 'a SequenceItem

    abstract Terminal: 'data -> Terminal -> Terminal

    default rewriter.Program data program =
        { Namespaces =
              program.Namespaces
              |> List.map (rewriter.Namespace data)
          Eof = rewriter.Terminal data program.Eof }

    default rewriter.Namespace data ns =
        { NamespaceKeyword = rewriter.Terminal data ns.NamespaceKeyword
          Name = rewriter.Terminal data ns.Name
          OpenBrace = rewriter.Terminal data ns.OpenBrace
          Elements =
              ns.Elements
              |> List.map (rewriter.NamespaceElement data)
          CloseBrace = rewriter.Terminal data ns.CloseBrace }

    default rewriter.NamespaceElement data (CallableDeclaration callable) =
        callable
        |> rewriter.CallableDeclaration data
        |> CallableDeclaration

    default rewriter.CallableDeclaration data callable =
        { CallableKeyword = rewriter.Terminal data callable.CallableKeyword
          Name = rewriter.Terminal data callable.Name
          Colon = rewriter.Terminal data callable.Colon
          ReturnType = rewriter.Type data callable.ReturnType
          OpenBrace = rewriter.Terminal data callable.OpenBrace
          Statements =
              callable.Statements
              |> List.map (rewriter.Statement data)
          CloseBrace = rewriter.Terminal data callable.CloseBrace }

    default rewriter.Type data ty =
        match ty with
        | MissingType terminal -> terminal |> rewriter.Terminal data |> MissingType
        | TypeParameter terminal ->
            terminal
            |> rewriter.Terminal data
            |> TypeParameter
        | BuiltInType terminal -> terminal |> rewriter.Terminal data |> BuiltInType
        | UserDefinedType terminal ->
            terminal
            |> rewriter.Terminal data
            |> UserDefinedType
        | TupleType tuple -> tuple |> rewriter.TupleType data |> TupleType
        | ArrayType array -> array |> rewriter.ArrayType data |> ArrayType
        | CallableType callable ->
            callable
            |> rewriter.CallableType data
            |> CallableType

    default rewriter.TupleType data tuple =
        { OpenParen = rewriter.Terminal data tuple.OpenParen
          Items =
              tuple.Items
              |> List.map (rewriter.SequenceItem data rewriter.Type)
          CloseParen = rewriter.Terminal data tuple.CloseParen }

    default rewriter.ArrayType data array =
        { BaseType = rewriter.Type data array.BaseType
          OpenBracket = rewriter.Terminal data array.OpenBracket
          CloseBracket = rewriter.Terminal data array.CloseBracket }

    default rewriter.CallableType data callable =
        { OpenParen = rewriter.Terminal data callable.OpenParen
          InnerOpenParen = rewriter.Terminal data callable.InnerOpenParen
          FromType = rewriter.Type data callable.FromType
          Arrow = rewriter.Terminal data callable.Arrow
          ToType = rewriter.Type data callable.ToType
          InnerCloseParen = rewriter.Terminal data callable.InnerCloseParen
          Characteristics =
              callable.Characteristics
              |> Option.map (rewriter.CharacteristicSection data)
          CloseParen = rewriter.Terminal data callable.CloseParen }

    default rewriter.CharacteristicSection data section =
        { IsKeyword = rewriter.Terminal data section.IsKeyword
          Characteristic = rewriter.Characteristic data section.Characteristic }

    default rewriter.CharacteristicGroup data group =
        { OpenParen = rewriter.Terminal data group.OpenParen
          Characteristic = rewriter.Characteristic data group.Characteristic
          CloseParen = rewriter.Terminal data group.CloseParen }

    default rewriter.CharacteristicBinaryOperator data operator =
        { Left = rewriter.Characteristic data operator.Left
          Operator = rewriter.Terminal data operator.Operator
          Right = rewriter.Characteristic data operator.Right }

    default rewriter.Characteristic data characteristic =
        match characteristic with
        | Adjoint -> Adjoint
        | Controlled -> Controlled
        | CharacteristicGroup group ->
            group
            |> rewriter.CharacteristicGroup data
            |> CharacteristicGroup
        | CharacteristicBinaryOperator operator ->
            operator
            |> rewriter.CharacteristicBinaryOperator data
            |> CharacteristicBinaryOperator

    default rewriter.Statement data statement =
        match statement with
        | Let lets -> lets |> rewriter.Let data |> Let
        | Return returns -> returns |> rewriter.Return data |> Return

    default rewriter.Let data lets =
        { LetKeyword = rewriter.Terminal data lets.LetKeyword
          Binding = rewriter.SymbolBinding data lets.Binding
          Equals = rewriter.Terminal data lets.Equals
          Value = rewriter.Expression data lets.Value
          Semicolon = rewriter.Terminal data lets.Semicolon }

    default rewriter.Return data returns =
        { ReturnKeyword = rewriter.Terminal data returns.ReturnKeyword
          Expression = rewriter.Expression data returns.Expression
          Semicolon = rewriter.Terminal data returns.Semicolon }

    default rewriter.SymbolBinding data binding =
        match binding with
        | SymbolName name -> rewriter.Terminal data name |> SymbolName
        | SymbolTuple bindings ->
            bindings
            |> List.map (rewriter.SymbolBinding data)
            |> SymbolTuple

    default rewriter.Expression data expression =
        match expression with
        | MissingExpression terminal ->
            terminal
            |> rewriter.Terminal data
            |> MissingExpression
        | Literal literal -> literal |> rewriter.Terminal data |> Literal
        | Tuple tuple -> tuple |> rewriter.Tuple data |> Tuple
        | BinaryOperator operator ->
            operator
            |> rewriter.BinaryOperator data
            |> BinaryOperator
        | Update update -> update |> rewriter.Update data |> Update

    default rewriter.Tuple data tuple =
        { OpenParen = rewriter.Terminal data tuple.OpenParen
          Items =
              tuple.Items
              |> List.map (rewriter.SequenceItem data rewriter.Expression)
          CloseParen = rewriter.Terminal data tuple.CloseParen }

    default rewriter.BinaryOperator data operator =
        { Left = rewriter.Expression data operator.Left
          Operator = rewriter.Terminal data operator.Operator
          Right = rewriter.Expression data operator.Right }

    default rewriter.Update data update =
        { Record = rewriter.Expression data update.Record
          With = rewriter.Terminal data update.With
          Item = rewriter.Expression data update.Item
          Arrow = rewriter.Terminal data update.Arrow
          Value = rewriter.Expression data update.Value }

    default rewriter.SequenceItem data f item =
        { Item = item.Item |> Option.map (f data)
          Comma = item.Comma |> Option.map (rewriter.Terminal data) }

    default _.Terminal _ terminal = terminal
