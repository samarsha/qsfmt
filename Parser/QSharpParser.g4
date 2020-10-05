parser grammar QSharpParser;

options {
    tokenVocab = QSharpLexer;
}

program : namespace* EOF;

// Namespace

namespace : 'namespace' qualifiedName BraceLeft namespaceElement* BraceRight;

qualifiedName : Identifier ('.' Identifier)*;

namespaceElement
    : openDirective
    | typeDeclaration
    | callableDeclaration
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
    : declarationPrefix ('function' | 'operation') Identifier parameterTuple ':' type characteristics? callableBody
    ;

parameterTuple : '(' (parameter (',' parameter)*)? ')';

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

specialization : specializationName specializationGenerator;

specializationName
    : 'body'
    | 'adjoint'
    | 'controlled'
    | 'controlled' 'adjoint'
    ;

specializationGenerator
    : 'auto'
    | 'self'
    | 'invert'
    | 'distribute'
    | 'intrinsic'
    | providedSpecialization
    ;

providedSpecialization : specializationParameterTuple scope;

specializationParameterTuple : '(' (specializationParameter (',' specializationParameter)*)? ')';

specializationParameter
    : Identifier
    | '...'
    ;

// Type

type
    : '_' # MissingType
    | Identifier # TypeName
    | '(' (type (',' type)*)? ')' # TupleType
    | '(' type ('->' | '=>') type characteristics? ')' # CallableType
    | type '[' ']' # ArrayType
    ;

// Statement

statement
    : expression ';' # ExpressionStatement
    | 'return' expression ';' # Return
    | 'fail' expression ';' # Fail
    | 'let' symbolTuple '=' expression ';' # Let
    | 'mutable' symbolTuple '=' expression ';' # Mutable
    | 'set' symbolTuple '=' expression ';' # Set
    | 'set' Identifier updateOperator expression ';' # SetUpdate
    | 'if' '(' expression ')' scope # If
    | 'elif' '(' expression ')' scope # Elif
    | 'else' scope # Else
    | 'for' '(' symbolTuple 'in' expression ')' scope # For
    | 'while' '(' expression ')' scope # While
    | 'repeat' scope # Repeat
    | 'until' '(' expression ')' 'fixup' scope # UntilFixup
    | 'within' scope # Within
    | 'apply' scope # Apply
    | 'using' '(' symbolTuple '=' qubitInitializer ')' scope # Using
    | 'borrowing' '(' symbolTuple '=' qubitInitializer ')' scope # Borrowing
    ;

scope : BraceLeft statement* BraceRight;

symbolTuple
    : Identifier # Symbol
    | '(' (symbolTuple (',' symbolTuple)*)? ')' # Symbols
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
    | '(' (qubitInitializer (',' qubitInitializer)*)? ')'
    ;

// Expression

expression
    : '_' # MissingExpression
    | qualifiedName ('<' (type (',' type)*)? '>')? # Identifier
    | Integer # Integer
    | BigInteger # BigInteger
    | Double # Double
    | DoubleQuote stringContent* StringDoubleQuote # String
    | DollarQuote interpStringContent* InterpDoubleQuote # InterpolatedString
    | boolLiteral # Bool
    | resultLiteral # Result
    | pauliLiteral # Pauli
    | '(' (expression (',' expression)*)? ')' # Tuple
    | '[' (expression (',' expression)*)? ']' # Array
    | 'new' type '[' expression ']' # NewArray
    | expression ('::' Identifier | '[' expression ']') # ItemAccess
    | expression '!' # Unwrap
    | <assoc=right> 'Controlled' expression # ControlledFunctor
    | <assoc=right> 'Adjoint' expression # AdjointFunctor
    | <assoc=right> expression '(' (expression (',' expression)*)? ')' # Call
    | <assoc=right> ('-' | 'not' | '~~~') expression # Negate
    | <assoc=right> expression '^' expression # Power
    | expression ('*' | '/' | '%') expression # MultiplyDivideModulo
    | expression ('+' | '-') expression # AddSubtract
    | expression ('>>>' | '<<<') expression # Shift
    | expression ('>' | '<' | '>=' | '<=') expression # GreaterLess
    | expression ('==' | '!=') expression # Equal
    | expression '&&&' expression # BitwiseAnd
    | expression '^^^' expression # BitwiseXor
    | expression '|||' expression # BitwiseOr
    | expression 'and' expression # And
    | expression 'or' expression # Or
    | <assoc=right> expression '?' expression '|' expression # Conditional
    | expression '..' expression # Range
    | expression '...' # RightOpenRange
    | '...' expression # LeftOpenRange
    | '...' # OpenRange
    | expression 'w/' expression '<-' expression # With
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
