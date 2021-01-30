namespace QsFmt.Formatter.SyntaxTree

open QsFmt.Formatter.Utils

/// <summary>
/// Rewrites a syntax tree.
/// </summary>
/// <typeparam name="context">The type of the context to use during recursive descent into the syntax tree.</typeparam>
type internal 'context Rewriter() =
    /// Rewrites a document node.
    abstract Document: 'context * Document -> Document

    /// Rewrites a namespace node.
    abstract Namespace: 'context * Namespace -> Namespace

    /// Rewrites a namespace item node.
    abstract NamespaceItem: 'context * NamespaceItem -> NamespaceItem

    /// Rewrites a callable declaration node.
    abstract CallableDeclaration: 'context * CallableDeclaration -> CallableDeclaration

    /// Rewrites a type node.
    abstract Type: 'context * Type -> Type

    /// Rewrites a type annotation node.
    abstract TypeAnnotation: 'context * TypeAnnotation -> TypeAnnotation

    /// Rewrites an array type node.
    abstract ArrayType: 'context * ArrayType -> ArrayType

    /// Rewrites a callable type node.
    abstract CallableType: 'context * CallableType -> CallableType

    /// Rewrites a callable characteristic section node.
    abstract CharacteristicSection: 'context * CharacteristicSection -> CharacteristicSection

    /// Rewrites a parenthesized characteristic node.
    abstract CharacteristicGroup: 'context * CharacteristicGroup -> CharacteristicGroup

    /// Rewrites a characteristic node.
    abstract Characteristic: 'context * Characteristic -> Characteristic

    /// Rewrites a statement node.
    abstract Statement: 'context * Statement -> Statement

    /// <summary>
    /// Rewrites a <c>let</c> statement node.
    /// </summary>
    abstract Let: 'context * Let -> Let

    /// <summary>
    /// Rewrites a <c>return</c> statement node.
    /// </summary>
    abstract Return: 'context * Return -> Return

    /// <summary>
    /// Rewrites an <c>if</c> statement node.
    /// </summary>
    abstract If: 'context * If -> If

    /// <summary>
    /// Rewrites an <c>else</c> statement node.
    /// </summary>
    abstract Else: 'context * Else -> Else

    /// Rewrites a symbol binding node.
    abstract SymbolBinding: 'context * SymbolBinding -> SymbolBinding

    /// Rewrites a symbol declaration node.
    abstract SymbolDeclaration: 'context * SymbolDeclaration -> SymbolDeclaration

    /// Rewrites an expression node.
    abstract Expression: 'context * Expression -> Expression

    /// Rewrites a copy-and-update expression node.
    abstract Update: 'context * Update -> Update

    /// Rewrites a block node, given a rewriter for the block contents.
    abstract Block: 'context * ('context * 'a -> 'a) * 'a Block -> 'a Block

    /// Rewrites a tuple node, given a rewriter for the tuple contents.
    abstract Tuple: 'context * ('context * 'a -> 'a) * 'a Tuple -> 'a Tuple

    /// Rewrites a sequence item node, given a rewriter for the sequence items.
    abstract SequenceItem: 'context * ('context * 'a -> 'a) * 'a SequenceItem -> 'a SequenceItem

    /// Rewrites a binary operator node, given a rewriter for the operands.
    abstract BinaryOperator: 'context * ('context * 'a -> 'a) * 'a BinaryOperator -> 'a BinaryOperator

    /// Rewrites a terminal node.
    abstract Terminal: 'context * Terminal -> Terminal

    default rewriter.Document(context, document) =
        { Namespaces =
              document.Namespaces
              |> List.map (curry rewriter.Namespace context)
          Eof = rewriter.Terminal(context, document.Eof) }

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
        { ItemType = rewriter.Type(context, array.ItemType)
          OpenBracket = rewriter.Terminal(context, array.OpenBracket)
          CloseBracket = rewriter.Terminal(context, array.CloseBracket) }

    default rewriter.CallableType(context, callable) =
        { FromType = rewriter.Type(context, callable.FromType)
          Arrow = rewriter.Terminal(context, callable.Arrow)
          ToType = rewriter.Type(context, callable.ToType)
          Characteristics =
              callable.Characteristics
              |> Option.map (curry rewriter.CharacteristicSection context) }

    default rewriter.CharacteristicSection(context, section) =
        { IsKeyword = rewriter.Terminal(context, section.IsKeyword)
          Characteristic = rewriter.Characteristic(context, section.Characteristic) }

    default rewriter.CharacteristicGroup(context, group) =
        { OpenParen = rewriter.Terminal(context, group.OpenParen)
          Characteristic = rewriter.Characteristic(context, group.Characteristic)
          CloseParen = rewriter.Terminal(context, group.CloseParen) }

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
            rewriter.BinaryOperator(context, rewriter.Characteristic, operator)
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
            rewriter.BinaryOperator(context, rewriter.Expression, operator)
            |> BinaryOperator
        | Update update -> rewriter.Update(context, update) |> Update
        | Expression.Unknown terminal ->
            rewriter.Terminal(context, terminal)
            |> Expression.Unknown

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

    default rewriter.BinaryOperator(context, mapper, operator) =
        { Left = mapper (context, operator.Left)
          Operator = rewriter.Terminal(context, operator.Operator)
          Right = mapper (context, operator.Right) }

    default _.Terminal(_, terminal) = terminal
