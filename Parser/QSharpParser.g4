parser grammar QSharpParser;

options {
    tokenVocab = QSharpLexer;
}

program : namespaces=namespace* eof=EOF;

// Namespace

namespace
    : keyword='namespace' name=qualifiedName openBrace=BraceLeft elements+=namespaceElement* closeBrace=BraceRight
    ;

qualifiedName : Identifier ('.' Identifier)*;

namespaceElement
    : openDirective # OpenElement
    | typeDeclaration # TypeElement
    | callable=callableDeclaration # CallableElement
    ;

// Open Directive

openDirective : 'open' qualifiedName ('as' qualifiedName)? ';';

// Declaration

attribute : '@' expression;

access : 'internal';

declarationPrefix : attribute* access?;

// Type Declaration

typeDeclaration : declarationPrefix 'newtype' Identifier '=' underlyingType ';';

underlyingType
    : typeDeclarationTuple
    | type
    ;

typeDeclarationTuple : '(' (typeTupleItem (',' typeTupleItem)*)? ')';

typeTupleItem
    : namedItem
    | underlyingType
    ;

namedItem : Identifier ':' type;

// Callable Declaration

callableDeclaration
    : declarationPrefix keyword=('function' | 'operation')
      name=Identifier typeParameterBinding? tuple=parameterTuple
      colon=':' returnType=type characteristics?
      body=callableBody
    ;

typeParameterBinding : '<' (TypeParameter (',' TypeParameter)*)? '>';

parameterTuple : openParen='(' (parameters+=parameter (commas+=',' parameters+=parameter)*)? closeParen=')';

parameter
    : namedItem
    | parameterTuple
    ;

characteristics : 'is' characteristicsExpression;

characteristicsExpression
    : 'Adj'
    | 'Ctl'
    | '(' characteristicsExpression ')'
    | characteristicsExpression '*' characteristicsExpression
    | characteristicsExpression '+' characteristicsExpression
    ;

callableBody
    : BraceLeft specialization* BraceRight
    | scope
    ;

specialization : specializationName+ specializationGenerator;

specializationName
    : 'body'
    | 'adjoint'
    | 'controlled'
    ;

specializationGenerator
    : 'auto' ';'
    | 'self' ';'
    | 'invert' ';'
    | 'distribute' ';'
    | 'intrinsic' ';'
    | providedSpecialization
    ;

providedSpecialization : specializationParameterTuple? scope;

specializationParameterTuple : '(' (specializationParameter (',' specializationParameter)*)? ')';

specializationParameter
    : Identifier
    | '...'
    ;

// Type

type
    : '_' # MissingType
    | TypeParameter # TypeParameter
    | 'BigInt' # BigIntType
    | 'Bool' # BoolType
    | 'Double' # DoubleType
    | 'Int' # IntType
    | 'Pauli' # PauliType
    | 'Qubit' # QubitType
    | 'Range' # RangeType
    | 'Result' # ResultType
    | 'String' # StringType
    | 'Unit' # UnitType
    | name=qualifiedName # UserDefinedType
    | '(' (type (',' type)* ','?)? ')' # TupleType
    | '(' arrowType characteristics? ')' # CallableType
    | type '[' ']' # ArrayType
    ;

arrowType
    : '(' type ('->' | '=>') type ')'
    | type ('->' | '=>') type
    ;

// Statement

statement
    : expression ';' # ExpressionStatement
    | return='return' value=expression semicolon=';' # ReturnStatement
    | 'fail' expression ';' # FailStatement
    | let='let' binding=symbolBinding equals='=' value=expression semicolon=';' # LetStatement
    | 'mutable' symbolBinding '=' expression ';' # MutableStatement
    | 'set' symbolBinding '=' expression ';' # SetStatement
    | 'set' Identifier updateOperator expression ';' # SetUpdateStatement
    | 'set' Identifier 'w/=' expression '<-' expression ';' # SetWithStatement
    | if='if' openParen='(' condition=expression closeParen=')' body=scope # IfStatement
    | 'elif' '(' expression ')' scope # ElifStatement
    | else='else' body=scope # ElseStatement
    | 'for' '(' symbolBinding 'in' expression ')' scope # ForStatement
    | 'while' '(' expression ')' scope # WhileStatement
    | 'repeat' scope # RepeatStatement
    | 'until' '(' expression ')' (';' | 'fixup' scope) # UntilStatement
    | 'within' scope # WithinStatement
    | 'apply' scope # ApplyStatement
    | 'using' '(' symbolBinding '=' qubitInitializer ')' scope # UsingStatement
    | 'borrowing' '(' symbolBinding '=' qubitInitializer ')' scope # BorrowingStatement
    ;

scope : openBrace=BraceLeft statements+=statement* closeBrace=BraceRight;

symbolBinding
    : '_' # DiscardSymbol
    | name=Identifier # SymbolName
    | openParen='(' (bindings+=symbolBinding (commas+=',' bindings+=symbolBinding)* ','?)? closeParen=')' # SymbolTuple
    ;

updateOperator
    : '^='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '>>>='
    | '<<<='
    | '&&&='
    | '^^^='
    | '|||='
    | 'and='
    | 'or='
    ;

qubitInitializer
    : 'Qubit' '(' ')'
    | 'Qubit' '[' expression ']'
    | '(' (qubitInitializer (',' qubitInitializer)* ','?)? ')'
    ;

// Expression

expression
    : '_' # MissingExpression
    | name=qualifiedName ('<' (type (',' type)* ','?)? '>')? # IdentifierExpression
    | value=IntegerLiteral # IntegerExpression
    | BigIntegerLiteral # BigIntegerExpression
    | DoubleLiteral # DoubleExpression
    | DoubleQuote stringContent* StringDoubleQuote # StringExpression
    | DollarQuote interpStringContent* InterpDoubleQuote # InterpStringExpression
    | boolLiteral # BoolExpression
    | resultLiteral # ResultExpression
    | pauliLiteral # PauliExpression
    | openParen='(' (items+=expression (commas+=',' items+=expression)* commas+=','?)? closeParen=')' # TupleExpression
    | '[' (expression (',' expression)* ','?)? ']' # ArrayExpression
    | 'new' type '[' expression ']' # NewArrayExpression
    | expression ('::' Identifier | '[' expression ']') # ItemAccessExpression
    | expression '!' # UnwrapExpression
    | <assoc=right> 'Controlled' expression # ControlledExpression
    | <assoc=right> 'Adjoint' expression # AdjointExpression
    | expression '(' (expression (',' expression)* ','?)? ')' # CallExpression
    | <assoc=right> ('-' | 'not' | '~~~') expression # NegationExpression
    | <assoc=right> expression '^' expression # ExponentExpression
    | expression ('*' | '/' | '%') expression # MultiplyExpression
    | left=expression operator=('+' | '-') right=expression # AddExpression
    | expression ('>>>' | '<<<') expression # ShiftExpression
    | expression ('>' | '<' | '>=' | '<=') expression # CompareExpression
    | left=expression operator=('==' | '!=') right=expression # EqualsExpression
    | expression '&&&' expression # BitwiseAndExpression
    | expression '^^^' expression # BitwiseXorExpression
    | expression '|||' expression # BitwiseOrExpression
    | expression 'and' expression # AndExpression
    | expression 'or' expression # OrExpression
    | <assoc=right> expression '?' expression '|' expression # ConditionalExpression
    | expression '..' expression # RangeExpression
    | expression '...' # RightOpenRangeExpression
    | '...' expression # LeftOpenRangeExpression
    | '...' # OpenRangeExpression
    | record=expression with='w/' item=expression arrow='<-' value=expression # UpdateExpression
    ;

boolLiteral
    : 'false'
    | 'true'
    ;

resultLiteral
    : 'Zero'
    | 'One'
    ;

pauliLiteral
    : 'PauliI'
    | 'PauliX'
    | 'PauliY'
    | 'PauliZ'
    ;

stringContent
    : StringEscape
    | StringText
    ;

interpStringContent
    : InterpStringEscape
    | InterpBraceLeft expression BraceRight
    | InterpStringText
    ;
