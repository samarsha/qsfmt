namespace QsFmt.Formatter.SyntaxTree

open QsFmt.Formatter.Utils

[<AbstractClass>]
type internal 'result Reducer() as reducer =
    let reduce = curry reducer.Combine |> List.reduce

    abstract Combine: 'result * 'result -> 'result

    abstract Program: Program -> 'result

    abstract Namespace: Namespace -> 'result

    abstract NamespaceItem: NamespaceItem -> 'result

    abstract CallableDeclaration: CallableDeclaration -> 'result

    abstract Type: Type -> 'result

    abstract TypeAnnotation: TypeAnnotation -> 'result

    abstract ArrayType: ArrayType -> 'result

    abstract CallableType: CallableType -> 'result

    abstract CharacteristicSection: CharacteristicSection -> 'result

    abstract CharacteristicGroup: CharacteristicGroup -> 'result

    abstract CharacteristicBinaryOperator: CharacteristicBinaryOperator -> 'result

    abstract Characteristic: Characteristic -> 'result

    abstract Statement: Statement -> 'result

    abstract Let: Let -> 'result

    abstract Return: Return -> 'result

    abstract If: If -> 'result

    abstract Else: Else -> 'result

    abstract SymbolBinding: SymbolBinding -> 'result

    abstract SymbolDeclaration: SymbolDeclaration -> 'result

    abstract Expression: Expression -> 'result

    abstract BinaryOperator: BinaryOperator -> 'result

    abstract Update: Update -> 'result

    abstract Block: ('a -> 'result) * 'a Block -> 'result

    abstract Tuple: ('a -> 'result) * 'a Tuple -> 'result

    abstract SequenceItem: ('a -> 'result) * 'a SequenceItem -> 'result

    abstract Terminal: Terminal -> 'result

    default _.Program program =
        (program.Namespaces |> List.map reducer.Namespace)
        @ [ reducer.Terminal program.Eof ]
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
        [ reducer.Type array.BaseType
          reducer.Terminal array.OpenBracket
          reducer.Terminal array.CloseBracket ]
        |> reduce

    default _.CallableType callable =
        [ reducer.Terminal callable.OpenParen
          reducer.Terminal callable.InnerOpenParen
          reducer.Type callable.FromType
          reducer.Terminal callable.Arrow
          reducer.Type callable.ToType
          reducer.Terminal callable.InnerCloseParen ]
        @ (callable.Characteristics
           |> Option.map reducer.CharacteristicSection
           |> Option.toList)
          @ [ reducer.Terminal callable.CloseParen ]
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

    default _.CharacteristicBinaryOperator operator =
        [ reducer.Characteristic operator.Left
          reducer.Terminal operator.Operator
          reducer.Characteristic operator.Right ]
        |> reduce

    default _.Characteristic characteristic =
        match characteristic with
        | Adjoint adjoint -> reducer.Terminal adjoint
        | Controlled controlled -> reducer.Terminal controlled
        | Group group -> reducer.CharacteristicGroup group
        | Characteristic.BinaryOperator operator -> reducer.CharacteristicBinaryOperator operator

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
          reducer.Terminal ifs.OpenParen
          reducer.Expression ifs.Condition
          reducer.Terminal ifs.CloseParen
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
        | BinaryOperator operator -> reducer.BinaryOperator operator
        | Update update -> reducer.Update update
        | Expression.Unknown terminal -> reducer.Terminal terminal

    default _.BinaryOperator operator =
        [ reducer.Expression operator.Left
          reducer.Terminal operator.Operator
          reducer.Expression operator.Right ]
        |> reduce

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
