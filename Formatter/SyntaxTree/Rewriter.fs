namespace QsFmt.Formatter.SyntaxTree.Rewriter

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type internal 'context Rewriter() =
    abstract Program: 'context -> Program -> Program

    abstract Namespace: 'context -> Namespace -> Namespace

    abstract NamespaceElement: 'context -> NamespaceElement -> NamespaceElement

    abstract CallableDeclaration: 'context -> CallableDeclaration -> CallableDeclaration

    abstract Type: 'context -> Type -> Type

    abstract TupleType: 'context -> TupleType -> TupleType

    abstract ArrayType: 'context -> ArrayType -> ArrayType

    abstract CallableType: 'context -> CallableType -> CallableType

    abstract CharacteristicSection: 'context -> CharacteristicSection -> CharacteristicSection

    abstract CharacteristicGroup: 'context -> CharacteristicGroup -> CharacteristicGroup

    abstract CharacteristicBinaryOperator: 'context -> CharacteristicBinaryOperator -> CharacteristicBinaryOperator

    abstract Characteristic: 'context -> Characteristic -> Characteristic

    abstract Statement: 'context -> Statement -> Statement

    abstract Let: 'context -> Let -> Let

    abstract Return: 'context -> Return -> Return

    abstract If: 'context -> If -> If

    abstract Else: 'context -> Else -> Else

    abstract SymbolBinding: 'context -> SymbolBinding -> SymbolBinding

    abstract Expression: 'context -> Expression -> Expression

    abstract Tuple: 'context -> Tuple -> Tuple

    abstract BinaryOperator: 'context -> BinaryOperator -> BinaryOperator

    abstract Update: 'context -> Update -> Update

    abstract SequenceItem: 'context -> ('context -> 'a -> 'a) -> 'a SequenceItem -> 'a SequenceItem

    abstract Terminal: 'context -> Terminal -> Terminal

    default rewriter.Program context program =
        { Namespaces =
              program.Namespaces
              |> List.map (rewriter.Namespace context)
          Eof = rewriter.Terminal context program.Eof }

    default rewriter.Namespace context ns =
        { NamespaceKeyword = rewriter.Terminal context ns.NamespaceKeyword
          Name = rewriter.Terminal context ns.Name
          OpenBrace = rewriter.Terminal context ns.OpenBrace
          Elements =
              ns.Elements
              |> List.map (rewriter.NamespaceElement context)
          CloseBrace = rewriter.Terminal context ns.CloseBrace }

    default rewriter.NamespaceElement context (CallableDeclaration callable) =
        callable
        |> rewriter.CallableDeclaration context
        |> CallableDeclaration

    default rewriter.CallableDeclaration context callable =
        { CallableKeyword = rewriter.Terminal context callable.CallableKeyword
          Name = rewriter.Terminal context callable.Name
          Colon = rewriter.Terminal context callable.Colon
          ReturnType = rewriter.Type context callable.ReturnType
          OpenBrace = rewriter.Terminal context callable.OpenBrace
          Statements =
              callable.Statements
              |> List.map (rewriter.Statement context)
          CloseBrace = rewriter.Terminal context callable.CloseBrace }

    default rewriter.Type context ty =
        match ty with
        | MissingType terminal ->
            terminal
            |> rewriter.Terminal context
            |> MissingType
        | TypeParameter terminal ->
            terminal
            |> rewriter.Terminal context
            |> TypeParameter
        | BuiltInType terminal ->
            terminal
            |> rewriter.Terminal context
            |> BuiltInType
        | UserDefinedType terminal ->
            terminal
            |> rewriter.Terminal context
            |> UserDefinedType
        | TupleType tuple -> tuple |> rewriter.TupleType context |> TupleType
        | ArrayType array -> array |> rewriter.ArrayType context |> ArrayType
        | CallableType callable ->
            callable
            |> rewriter.CallableType context
            |> CallableType

    default rewriter.TupleType context tuple =
        { OpenParen = rewriter.Terminal context tuple.OpenParen
          Items =
              tuple.Items
              |> List.map (rewriter.SequenceItem context rewriter.Type)
          CloseParen = rewriter.Terminal context tuple.CloseParen }

    default rewriter.ArrayType context array =
        { BaseType = rewriter.Type context array.BaseType
          OpenBracket = rewriter.Terminal context array.OpenBracket
          CloseBracket = rewriter.Terminal context array.CloseBracket }

    default rewriter.CallableType context callable =
        { OpenParen = rewriter.Terminal context callable.OpenParen
          InnerOpenParen = rewriter.Terminal context callable.InnerOpenParen
          FromType = rewriter.Type context callable.FromType
          Arrow = rewriter.Terminal context callable.Arrow
          ToType = rewriter.Type context callable.ToType
          InnerCloseParen = rewriter.Terminal context callable.InnerCloseParen
          Characteristics =
              callable.Characteristics
              |> Option.map (rewriter.CharacteristicSection context)
          CloseParen = rewriter.Terminal context callable.CloseParen }

    default rewriter.CharacteristicSection context section =
        { IsKeyword = rewriter.Terminal context section.IsKeyword
          Characteristic = rewriter.Characteristic context section.Characteristic }

    default rewriter.CharacteristicGroup context group =
        { OpenParen = rewriter.Terminal context group.OpenParen
          Characteristic = rewriter.Characteristic context group.Characteristic
          CloseParen = rewriter.Terminal context group.CloseParen }

    default rewriter.CharacteristicBinaryOperator context operator =
        { Left = rewriter.Characteristic context operator.Left
          Operator = rewriter.Terminal context operator.Operator
          Right = rewriter.Characteristic context operator.Right }

    default rewriter.Characteristic context characteristic =
        match characteristic with
        | Adjoint -> Adjoint
        | Controlled -> Controlled
        | CharacteristicGroup group ->
            group
            |> rewriter.CharacteristicGroup context
            |> CharacteristicGroup
        | CharacteristicBinaryOperator operator ->
            operator
            |> rewriter.CharacteristicBinaryOperator context
            |> CharacteristicBinaryOperator

    default rewriter.Statement context statement =
        match statement with
        | Let lets -> lets |> rewriter.Let context |> Let
        | Return returns -> returns |> rewriter.Return context |> Return
        | If ifs -> ifs |> rewriter.If context |> If
        | Else elses -> elses |> rewriter.Else context |> Else

    default rewriter.Let context lets =
        { LetKeyword = rewriter.Terminal context lets.LetKeyword
          Binding = rewriter.SymbolBinding context lets.Binding
          Equals = rewriter.Terminal context lets.Equals
          Value = rewriter.Expression context lets.Value
          Semicolon = rewriter.Terminal context lets.Semicolon }

    default rewriter.Return context returns =
        { ReturnKeyword = rewriter.Terminal context returns.ReturnKeyword
          Expression = rewriter.Expression context returns.Expression
          Semicolon = rewriter.Terminal context returns.Semicolon }

    default rewriter.If context ifs =
        { IfKeyword = rewriter.Terminal context ifs.IfKeyword
          OpenParen = rewriter.Terminal context ifs.OpenParen
          Condition = rewriter.Expression context ifs.Condition
          CloseParen = rewriter.Terminal context ifs.CloseParen
          OpenBrace = rewriter.Terminal context ifs.OpenBrace
          Statements =
              ifs.Statements
              |> List.map (rewriter.Statement context)
          CloseBrace = rewriter.Terminal context ifs.CloseBrace }

    default rewriter.Else context elses =
        { ElseKeyword = rewriter.Terminal context elses.ElseKeyword
          OpenBrace = rewriter.Terminal context elses.OpenBrace
          Statements =
              elses.Statements
              |> List.map (rewriter.Statement context)
          CloseBrace = rewriter.Terminal context elses.CloseBrace }

    default rewriter.SymbolBinding context binding =
        match binding with
        | SymbolName name -> rewriter.Terminal context name |> SymbolName
        | SymbolTuple bindings ->
            bindings
            |> List.map (rewriter.SymbolBinding context)
            |> SymbolTuple

    default rewriter.Expression context expression =
        match expression with
        | MissingExpression terminal ->
            terminal
            |> rewriter.Terminal context
            |> MissingExpression
        | Literal literal -> literal |> rewriter.Terminal context |> Literal
        | Tuple tuple -> tuple |> rewriter.Tuple context |> Tuple
        | BinaryOperator operator ->
            operator
            |> rewriter.BinaryOperator context
            |> BinaryOperator
        | Update update -> update |> rewriter.Update context |> Update

    default rewriter.Tuple context tuple =
        { OpenParen = rewriter.Terminal context tuple.OpenParen
          Items =
              tuple.Items
              |> List.map (rewriter.SequenceItem context rewriter.Expression)
          CloseParen = rewriter.Terminal context tuple.CloseParen }

    default rewriter.BinaryOperator context operator =
        { Left = rewriter.Expression context operator.Left
          Operator = rewriter.Terminal context operator.Operator
          Right = rewriter.Expression context operator.Right }

    default rewriter.Update context update =
        { Record = rewriter.Expression context update.Record
          With = rewriter.Terminal context update.With
          Item = rewriter.Expression context update.Item
          Arrow = rewriter.Terminal context update.Arrow
          Value = rewriter.Expression context update.Value }

    default rewriter.SequenceItem context mapper item =
        { Item = item.Item |> Option.map (mapper context)
          Comma =
              item.Comma
              |> Option.map (rewriter.Terminal context) }

    default _.Terminal _ terminal = terminal
