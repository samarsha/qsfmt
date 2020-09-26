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

Whitespace : [ \n\r\t]+ -> channel(HIDDEN);

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
    : declarationPrefix 'function' callableDeclarationSuffix
    | declarationPrefix 'operation' callableDeclarationSuffix
    ;

callableDeclarationSuffix : Identifier parameterTuple ':' type characteristics? callableBody;

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
    : Identifier
    | '(' (type (',' type)*)? ')'
    | '(' type '->' type ')'
    | '(' type '=>' type characteristics? ')'
    | type '[]'
    ;

// Statement

statement
    : expression ';'
    | 'return' expression ';'
    | 'fail' expression ';'
    | 'let' symbolTuple '=' expression ';'
    | 'mutable' symbolTuple '=' expression ';'
    | 'set' symbolTuple '=' expression ';'
    | 'set' Identifier updateOperator expression ';'
    | 'if' '(' expression ')' scope ('elif' '(' expression ')' scope)* ('else' scope)?
    | 'for' '(' symbolTuple 'in' expression ')' scope ';'
    | 'while' '(' expression ')' scope ';'
    | 'repeat' scope 'until' '(' expression ')' 'fixup' scope
    | 'within' scope 'apply' scope ';'
    | 'using' '(' symbolTuple '=' qubitInitializer ')' scope
    | 'borrowing' '(' symbolTuple '=' qubitInitializer ')' scope
    ;

scope : '{' statement* '}';

symbolTuple
    : Identifier
    | '(' (symbolTuple (',' symbolTuple)*)? ')'
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
    : '_'
    | qualifiedName ('<' type (',' type)* '>')?
    | Integer
    | BigInteger
    | Double
    | String
    | boolLiteral
    | resultLiteral
    | pauliLiteral
    | '(' (expression (',' expression)*)? ')'
    | '[' (expression (',' expression)*)? ']'
    | 'new' type '[' expression ']'
    | expression '::' Identifier
    | expression '[' expression ']'
    | expression '!'
    | functor expression
    | expression '(' (expression (',' expression)*)? ')'
    | '-' expression
    | 'not' expression
    | '~~~' expression
    | expression '^' expression
    | expression '*' expression
    | expression '/' expression
    | expression '%' expression
    | expression '+' expression
    | expression '-' expression
    | expression '>>>' expression
    | expression '<<<' expression
    | expression '>' expression
    | expression '<' expression
    | expression '>=' expression
    | expression '<=' expression
    | expression '!=' expression
    | expression '==' expression
    | expression '&&&' expression
    | expression '^^^' expression
    | expression '|||' expression
    | expression 'and' expression
    | expression 'or' expression
    | expression '?' expression '|' expression
    | expression '..' expression
    | expression '...'
    | '...' expression
    | '...'
    | expression 'w/' expression '<-' expression
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
