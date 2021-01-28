namespace QsFmt.Formatter.SyntaxTree

open QsFmt.Formatter.Utils

type internal 'context Rewriter() =
    abstract Program: 'context * Program -> Program

    abstract Namespace: 'context * Namespace -> Namespace

    abstract NamespaceItem: 'context * NamespaceItem -> NamespaceItem

    abstract CallableDeclaration: 'context * CallableDeclaration -> CallableDeclaration

    abstract Type: 'context * Type -> Type

    abstract TypeAnnotation: 'context * TypeAnnotation -> TypeAnnotation

    abstract ArrayType: 'context * ArrayType -> ArrayType

    abstract CallableType: 'context * CallableType -> CallableType

    abstract CharacteristicSection: 'context * CharacteristicSection -> CharacteristicSection

    abstract CharacteristicGroup: 'context * CharacteristicGroup -> CharacteristicGroup

    abstract CharacteristicBinaryOperator: 'context * CharacteristicBinaryOperator -> CharacteristicBinaryOperator

    abstract Characteristic: 'context * Characteristic -> Characteristic

    abstract Statement: 'context * Statement -> Statement

    abstract Let: 'context * Let -> Let

    abstract Return: 'context * Return -> Return

    abstract If: 'context * If -> If

    abstract Else: 'context * Else -> Else

    abstract SymbolBinding: 'context * SymbolBinding -> SymbolBinding

    abstract SymbolDeclaration: 'context * SymbolDeclaration -> SymbolDeclaration

    abstract Expression: 'context * Expression -> Expression

    abstract BinaryOperator: 'context * BinaryOperator -> BinaryOperator

    abstract Update: 'context * Update -> Update

    abstract Block: 'context * ('context * 'a -> 'a) * 'a Block -> 'a Block

    abstract Tuple: 'context * ('context * 'a -> 'a) * 'a Tuple -> 'a Tuple

    abstract SequenceItem: 'context * ('context * 'a -> 'a) * 'a SequenceItem -> 'a SequenceItem

    abstract Terminal: 'context * Terminal -> Terminal

    default rewriter.Program(context, program) =
        { Namespaces =
              program.Namespaces
              |> List.map (curry rewriter.Namespace context)
          Eof = rewriter.Terminal(context, program.Eof) }

    default rewriter.Namespace(context, ns) =
        { NamespaceKeyword = rewriter.Terminal(context, ns.NamespaceKeyword)
          Name = rewriter.Terminal(context, ns.Name)
          Block = rewriter.Block(context, rewriter.NamespaceItem, ns.Block) }

    default rewriter.NamespaceItem(context, item) =
        match item with
        | CallableDeclaration callable ->
            rewriter.CallableDeclaration(context, callable)
            |> CallableDeclaration
        | Unknown terminal -> rewriter.Terminal(context, terminal) |> Unknown

    default rewriter.CallableDeclaration(context, callable) =
        { CallableKeyword = rewriter.Terminal(context, callable.CallableKeyword)
          Name = rewriter.Terminal(context, callable.Name)
          Parameters = rewriter.SymbolBinding(context, callable.Parameters)
          ReturnType = rewriter.TypeAnnotation(context, callable.ReturnType)
          Block = rewriter.Block(context, rewriter.Statement, callable.Block) }

    default rewriter.Type(context, typ) =
        match typ with
        | Type.Missing missing ->
            rewriter.Terminal(context, missing)
            |> Type.Missing
        | Parameter name -> rewriter.Terminal(context, name) |> Parameter
        | BuiltIn name -> rewriter.Terminal(context, name) |> BuiltIn
        | UserDefined name -> rewriter.Terminal(context, name) |> UserDefined
        | Type.Tuple tuple ->
            rewriter.Tuple(context, rewriter.Type, tuple)
            |> Type.Tuple
        | Array array -> rewriter.ArrayType(context, array) |> Array
        | Type.Callable callable ->
            rewriter.CallableType(context, callable)
            |> Type.Callable
        | Type.Unknown terminal ->
            rewriter.Terminal(context, terminal)
            |> Type.Unknown

    default rewriter.TypeAnnotation(context, annotation) =
        { Colon = rewriter.Terminal(context, annotation.Colon)
          Type = rewriter.Type(context, annotation.Type) }

    default rewriter.ArrayType(context, array) =
        { BaseType = rewriter.Type(context, array.BaseType)
          OpenBracket = rewriter.Terminal(context, array.OpenBracket)
          CloseBracket = rewriter.Terminal(context, array.CloseBracket) }

    default rewriter.CallableType(context, callable) =
        { OpenParen = rewriter.Terminal(context, callable.OpenParen)
          InnerOpenParen = rewriter.Terminal(context, callable.InnerOpenParen)
          FromType = rewriter.Type(context, callable.FromType)
          Arrow = rewriter.Terminal(context, callable.Arrow)
          ToType = rewriter.Type(context, callable.ToType)
          InnerCloseParen = rewriter.Terminal(context, callable.InnerCloseParen)
          Characteristics =
              callable.Characteristics
              |> Option.map (curry rewriter.CharacteristicSection context)
          CloseParen = rewriter.Terminal(context, callable.CloseParen) }

    default rewriter.CharacteristicSection(context, section) =
        { IsKeyword = rewriter.Terminal(context, section.IsKeyword)
          Characteristic = rewriter.Characteristic(context, section.Characteristic) }

    default rewriter.CharacteristicGroup(context, group) =
        { OpenParen = rewriter.Terminal(context, group.OpenParen)
          Characteristic = rewriter.Characteristic(context, group.Characteristic)
          CloseParen = rewriter.Terminal(context, group.CloseParen) }

    default rewriter.CharacteristicBinaryOperator(context, operator) =
        { Left = rewriter.Characteristic(context, operator.Left)
          Operator = rewriter.Terminal(context, operator.Operator)
          Right = rewriter.Characteristic(context, operator.Right) }

    default rewriter.Characteristic(context, characteristic) =
        match characteristic with
        | Adjoint adjoint -> rewriter.Terminal(context, adjoint) |> Adjoint
        | Controlled controlled ->
            rewriter.Terminal(context, controlled)
            |> Controlled
        | Group group ->
            rewriter.CharacteristicGroup(context, group)
            |> Group
        | Characteristic.BinaryOperator operator ->
            rewriter.CharacteristicBinaryOperator(context, operator)
            |> Characteristic.BinaryOperator

    default rewriter.Statement(context, statement) =
        match statement with
        | Let lets -> rewriter.Let(context, lets) |> Let
        | Return returns -> rewriter.Return(context, returns) |> Return
        | If ifs -> rewriter.If(context, ifs) |> If
        | Else elses -> rewriter.Else(context, elses) |> Else
        | Statement.Unknown terminal ->
            rewriter.Terminal(context, terminal)
            |> Statement.Unknown

    default rewriter.Let(context, lets) =
        { LetKeyword = rewriter.Terminal(context, lets.LetKeyword)
          Binding = rewriter.SymbolBinding(context, lets.Binding)
          Equals = rewriter.Terminal(context, lets.Equals)
          Value = rewriter.Expression(context, lets.Value)
          Semicolon = rewriter.Terminal(context, lets.Semicolon) }

    default rewriter.Return(context, returns) =
        { ReturnKeyword = rewriter.Terminal(context, returns.ReturnKeyword)
          Expression = rewriter.Expression(context, returns.Expression)
          Semicolon = rewriter.Terminal(context, returns.Semicolon) }

    default rewriter.If(context, ifs) =
        { IfKeyword = rewriter.Terminal(context, ifs.IfKeyword)
          Condition = rewriter.Expression(context, ifs.Condition)
          Block = rewriter.Block(context, rewriter.Statement, ifs.Block) }

    default rewriter.Else(context, elses) =
        { ElseKeyword = rewriter.Terminal(context, elses.ElseKeyword)
          Block = rewriter.Block(context, rewriter.Statement, elses.Block) }

    default rewriter.SymbolBinding(context, binding) =
        match binding with
        | SymbolDeclaration declaration ->
            rewriter.SymbolDeclaration(context, declaration)
            |> SymbolDeclaration
        | SymbolTuple tuple ->
            rewriter.Tuple(context, rewriter.SymbolBinding, tuple)
            |> SymbolTuple

    default rewriter.SymbolDeclaration(context, declaration) =
        { Name = rewriter.Terminal(context, declaration.Name)
          Type =
              declaration.Type
              |> Option.map (curry rewriter.TypeAnnotation context) }

    default rewriter.Expression(context, expression) =
        match expression with
        | Missing terminal -> rewriter.Terminal(context, terminal) |> Missing
        | Literal literal -> rewriter.Terminal(context, literal) |> Literal
        | Tuple tuple ->
            rewriter.Tuple(context, rewriter.Expression, tuple)
            |> Tuple
        | BinaryOperator operator ->
            rewriter.BinaryOperator(context, operator)
            |> BinaryOperator
        | Update update -> rewriter.Update(context, update) |> Update
        | Expression.Unknown terminal ->
            rewriter.Terminal(context, terminal)
            |> Expression.Unknown

    default rewriter.BinaryOperator(context, operator) =
        { Left = rewriter.Expression(context, operator.Left)
          Operator = rewriter.Terminal(context, operator.Operator)
          Right = rewriter.Expression(context, operator.Right) }

    default rewriter.Update(context, update) =
        { Record = rewriter.Expression(context, update.Record)
          With = rewriter.Terminal(context, update.With)
          Item = rewriter.Expression(context, update.Item)
          Arrow = rewriter.Terminal(context, update.Arrow)
          Value = rewriter.Expression(context, update.Value) }

    default rewriter.Block(context, mapper, block) =
        { OpenBrace = rewriter.Terminal(context, block.OpenBrace)
          Items = block.Items |> List.map (curry mapper context)
          CloseBrace = rewriter.Terminal(context, block.CloseBrace) }

    default rewriter.Tuple(context, mapper, tuple) =
        { OpenParen = rewriter.Terminal(context, tuple.OpenParen)
          Items =
              tuple.Items
              |> List.map (fun item -> rewriter.SequenceItem(context, mapper, item))
          CloseParen = rewriter.Terminal(context, tuple.CloseParen) }

    default rewriter.SequenceItem(context, mapper, item) =
        { Item = item.Item |> Option.map (curry mapper context)
          Comma =
              item.Comma
              |> Option.map (curry rewriter.Terminal context) }

    default _.Terminal(_, terminal) = terminal
