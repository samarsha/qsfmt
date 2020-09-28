grammar QSharp;

// Lexer Rules

Integer
    : [0-9]+
    | ('0x' | '0X') [0-9a-fA-F]+
    | ('0o' | '0O') [0-7]+
    | ('0b' | '0B') [0-1]+
    ;

BigInteger : Integer ('L' | 'l');

Double
    : [0-9]+ '.' [0-9]+
    | '.' [0-9]+
    | [0-9]+ '.'
    | [0-9]+ ('e' | 'E') [0-9]+
    ;

Identifier : IdentifierStart IdentifierContinuation*;

IdentifierStart
    : '_'
    | [\p{Letter}]
    | [\p{Letter_Number}]
    ;

IdentifierContinuation
    : [\p{Connector_Punctuation}]
    | [\p{Decimal_Number}]
    | [\p{Format}]
    | [\p{Letter}]
    | [\p{Letter_Number}]
    | [\p{Nonspacing_Mark}]
    | [\p{Spacing_Mark}]
    ;

// TODO: Interpolated strings.
String : '"' (~'"' | '\\"')* '"';

Whitespace : (' ' | '\n' | '\r' | '\t')+ -> channel(HIDDEN);

Comment : '//' ~('\n' | '\r')* -> channel(HIDDEN);

Invalid : . -> channel(HIDDEN);

// Program

program : namespace* EOF;

// Namespace

namespace : 'namespace' qualifiedName '{' namespaceElement* '}';

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

typeDeclarationTuple : '(' typeTupleItem (',' typeTupleItem)* ')';

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
    : '{' specialization* '}'
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
    : Identifier # TypeName
    | '(' (type (',' type)*)? ')' # TupleType
    | '(' type ('->' | '=>') type characteristics? ')' # CallableType
    | type '[]' # ArrayType
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

scope : '{' statement* '}';

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

// TODO: Operator precedence and associativity.
expression
    : '_' # MissingExpression
    | qualifiedName ('<' type (',' type)* '>')? # Identifier
    | Integer # Integer
    | BigInteger # BigInteger
    | Double # Double
    | String # String
    | boolLiteral # Bool
    | resultLiteral # Result
    | pauliLiteral # Pauli
    | '(' (expression (',' expression)*)? ')' # Tuple
    | '[' (expression (',' expression)*)? ']' # Array
    | 'new' type '[' expression ']' # NewArray
    | expression '::' Identifier # AccessNamedItem
    | expression '[' expression ']' # AccessIndex
    | expression '!' # Unwrap
    | functor expression # ApplyFunctor
    | expression '(' (expression (',' expression)*)? ')' # Call
    | '-' expression # Negate
    | 'not' expression # Not
    | '~~~' expression # BitwiseNot
    | expression '^' expression # Power
    | expression '*' expression # Multiply
    | expression '/' expression # Divide
    | expression '%' expression # Modulo
    | expression '+' expression # Add
    | expression '-' expression # Subtract
    | expression '>>>' expression # ShiftRight
    | expression '<<<' expression # ShiftLeft
    | expression '>' expression # Greater
    | expression '<' expression # Less
    | expression '>=' expression # GreaterEqual
    | expression '<=' expression # LessEqual
    | expression '!=' expression # NotEqual
    | expression '==' expression # Equal
    | expression '&&&' expression # BitwiseAnd
    | expression '^^^' expression # BitwiseXor
    | expression '|||' expression # BitwiseOr
    | expression 'and' expression # And
    | expression 'or' expression # Or
    | expression '?' expression '|' expression # Conditional
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

functor
    : 'Adjoint'
    | 'Controlled'
    ;
