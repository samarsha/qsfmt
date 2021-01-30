﻿namespace QsFmt.Formatter.SyntaxTree

open QsFmt.Formatter.Utils

/// <summary>
/// Reduces a syntax tree to a single value.
/// </summary>
/// <typeparam name="result">The type of the reduced result.</typeparam>
[<AbstractClass>]
type internal 'result Reducer() as reducer =
    /// Reduces a list of results into a single result.
    let reduce = curry reducer.Combine |> List.reduce

    /// Combines two results into a single result.
    abstract Combine: 'result * 'result -> 'result

    /// Reduces a document node.
    abstract Document: Document -> 'result

    /// Reduces a namespace node.
    abstract Namespace: Namespace -> 'result

    /// Reduces a namespace item node.
    abstract NamespaceItem: NamespaceItem -> 'result

    /// Reduces a callable declaration node.
    abstract CallableDeclaration: CallableDeclaration -> 'result

    /// Reduces a type node.
    abstract Type: Type -> 'result

    /// Reduces a type annotation node.
    abstract TypeAnnotation: TypeAnnotation -> 'result

    /// Reduces an array type node.
    abstract ArrayType: ArrayType -> 'result

    /// Reduces a callable type node.
    abstract CallableType: CallableType -> 'result

    /// Reduces a callable characteristic section node.
    abstract CharacteristicSection: CharacteristicSection -> 'result

    /// Reduces a parenthesized characteristic node.
    abstract CharacteristicGroup: CharacteristicGroup -> 'result

    /// Reduces a characteristic node.
    abstract Characteristic: Characteristic -> 'result

    /// Reduces a statement node.
    abstract Statement: Statement -> 'result

    /// <summary>
    /// Reduces a <c>let</c> statement node.
    /// </summary>
    abstract Let: Let -> 'result

    /// <summary>
    /// Reduces a <c>return</c> statement node.
    /// </summary>
    abstract Return: Return -> 'result

    /// <summary>
    /// Reduces an <c>if</c> statement node.
    /// </summary>
    abstract If: If -> 'result

    /// <summary>
    /// Reduces an <c>else</c> statement node.
    /// </summary>
    abstract Else: Else -> 'result

    /// Reduces a symbol binding node.
    abstract SymbolBinding: SymbolBinding -> 'result

    /// Reduces a symbol declaration node.
    abstract SymbolDeclaration: SymbolDeclaration -> 'result

    /// Reduces an expression node.
    abstract Expression: Expression -> 'result

    /// Reduces a copy-and-update expression node.
    abstract Update: Update -> 'result

    /// Reduces a block node, given a reducer for the block contents.
    abstract Block: ('a -> 'result) * 'a Block -> 'result

    /// Reduces a tuple node, given a reducer for the tuple contents.
    abstract Tuple: ('a -> 'result) * 'a Tuple -> 'result

    /// Reduces a sequence item node, given a reducer for the sequence items.
    abstract SequenceItem: ('a -> 'result) * 'a SequenceItem -> 'result

    /// Reduces a binary operator node, given a reducer for the operands.
    abstract BinaryOperator: ('a -> 'result) * 'a BinaryOperator -> 'result

    /// Reduces a terminal node.
    abstract Terminal: Terminal -> 'result

    default _.Document document =
        (document.Namespaces |> List.map reducer.Namespace)
        @ [ reducer.Terminal document.Eof ]
        |> reduce

    default _.Namespace ns =
        [ reducer.Terminal ns.NamespaceKeyword
          reducer.Terminal ns.Name
          reducer.Block(reducer.NamespaceItem, ns.Block) ]
        |> reduce

    default _.NamespaceItem item =
        match item with
        | CallableDeclaration callable -> reducer.CallableDeclaration callable
        | Unknown terminal -> reducer.Terminal terminal

    default _.CallableDeclaration callable =
        [ reducer.Terminal callable.CallableKeyword
          reducer.Terminal callable.Name
          reducer.SymbolBinding callable.Parameters
          reducer.TypeAnnotation callable.ReturnType
          reducer.Block(reducer.Statement, callable.Block) ]
        |> reduce

    default _.Type typ =
        match typ with
        | Type.Missing missing -> reducer.Terminal missing
        | Parameter name
        | BuiltIn name
        | UserDefined name -> reducer.Terminal name
        | Type.Tuple tuple -> reducer.Tuple(reducer.Type, tuple)
        | Array array -> reducer.ArrayType array
        | Callable callable -> reducer.CallableType callable
        | Type.Unknown terminal -> reducer.Terminal terminal

    default _.TypeAnnotation annotation =
        [ reducer.Terminal annotation.Colon
          reducer.Type annotation.Type ]
        |> reduce

    default _.ArrayType array =
        [ reducer.Type array.ItemType
          reducer.Terminal array.OpenBracket
          reducer.Terminal array.CloseBracket ]
        |> reduce

    default _.CallableType callable =
        [ reducer.Type callable.FromType
          reducer.Terminal callable.Arrow
          reducer.Type callable.ToType ]
        @ (callable.Characteristics
           |> Option.map reducer.CharacteristicSection
           |> Option.toList)
        |> reduce

    default _.CharacteristicSection section =
        [ reducer.Terminal section.IsKeyword
          reducer.Characteristic section.Characteristic ]
        |> reduce

    default _.CharacteristicGroup group =
        [ reducer.Terminal group.OpenParen
          reducer.Characteristic group.Characteristic
          reducer.Terminal group.CloseParen ]
        |> reduce

    default _.Characteristic characteristic =
        match characteristic with
        | Adjoint adjoint -> reducer.Terminal adjoint
        | Controlled controlled -> reducer.Terminal controlled
        | Group group -> reducer.CharacteristicGroup group
        | Characteristic.BinaryOperator operator -> reducer.BinaryOperator(reducer.Characteristic, operator)

    default _.Statement statement =
        match statement with
        | Let lets -> reducer.Let lets
        | Return returns -> reducer.Return returns
        | If ifs -> reducer.If ifs
        | Else elses -> reducer.Else elses
        | Statement.Unknown terminal -> reducer.Terminal terminal

    default _.Let lets =
        [ reducer.Terminal lets.LetKeyword
          reducer.SymbolBinding lets.Binding
          reducer.Terminal lets.Equals
          reducer.Expression lets.Value
          reducer.Terminal lets.Semicolon ]
        |> reduce

    default _.Return returns =
        [ reducer.Terminal returns.ReturnKeyword
          reducer.Expression returns.Expression
          reducer.Terminal returns.Semicolon ]
        |> reduce

    default _.If ifs =
        [ reducer.Terminal ifs.IfKeyword
          reducer.Expression ifs.Condition
          reducer.Block(reducer.Statement, ifs.Block) ]
        |> reduce

    default _.Else elses =
        [ reducer.Terminal elses.ElseKeyword
          reducer.Block(reducer.Statement, elses.Block) ]
        |> reduce

    default _.SymbolBinding binding =
        match binding with
        | SymbolDeclaration declaration -> reducer.SymbolDeclaration declaration
        | SymbolTuple tuple -> reducer.Tuple(reducer.SymbolBinding, tuple)

    default _.SymbolDeclaration declaration =
        reducer.Terminal declaration.Name
        :: (declaration.Type
            |> Option.map reducer.TypeAnnotation
            |> Option.toList)
        |> reduce

    default _.Expression expression =
        match expression with
        | Missing terminal -> reducer.Terminal terminal
        | Literal literal -> reducer.Terminal literal
        | Tuple tuple -> reducer.Tuple(reducer.Expression, tuple)
        | BinaryOperator operator -> reducer.BinaryOperator(reducer.Expression, operator)
        | Update update -> reducer.Update update
        | Expression.Unknown terminal -> reducer.Terminal terminal

    default _.Update update =
        [ reducer.Expression update.Record
          reducer.Terminal update.With
          reducer.Expression update.Item
          reducer.Terminal update.Arrow
          reducer.Expression update.Value ]
        |> reduce

    default _.Block(mapper, block) =
        reducer.Terminal block.OpenBrace
        :: (block.Items |> List.map mapper)
        @ [ reducer.Terminal block.CloseBrace ]
        |> reduce

    default _.Tuple(mapper, tuple) =
        reducer.Terminal tuple.OpenParen
        :: (tuple.Items
            |> List.map (curry reducer.SequenceItem mapper))
        @ [ reducer.Terminal tuple.CloseParen ]
        |> reduce

    default _.SequenceItem(mapper, item) =
        (item.Item |> Option.map mapper |> Option.toList)
        @ (item.Comma
           |> Option.map reducer.Terminal
           |> Option.toList)
        |> reduce

    default _.BinaryOperator(mapper, operator) =
        [ mapper operator.Left
          reducer.Terminal operator.Operator
          mapper operator.Right ]
        |> reduce
