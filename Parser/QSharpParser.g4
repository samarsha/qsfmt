parser grammar QSharpParser;

options {
    tokenVocab = QSharpLexer;
}

program : namespace* EOF;

// Namespace

namespace : 'namespace' qualifiedName BraceLeft namespaceElement* BraceRight;

qualifiedName : Identifier ('.' Identifier)*;

namespaceElement
    : openDirective # OpenElement
    | typeDeclaration # TypeElement
    | callableDeclaration # CallableElement
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
    : declarationPrefix ('function' | 'operation')
      Identifier typeParameterBinding? parameterTuple
      ':' type characteristics?
      callableBody
    ;

typeParameterBinding : '<' (TypeParameter (',' TypeParameter)*)? '>';

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
    | qualifiedName # UserDefinedType
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
    | 'return' expression ';' # ReturnStatement
    | 'fail' expression ';' # FailStatement
    | 'let' symbolBinding '=' expression ';' # LetStatement
    | 'mutable' symbolBinding '=' expression ';' # MutableStatement
    | 'set' symbolBinding '=' expression ';' # SetStatement
    | 'set' Identifier updateOperator expression ';' # SetUpdateStatement
    | 'set' Identifier 'w/=' expression '<-' expression ';' # SetWithStatement
    | 'if' '(' expression ')' scope # IfStatement
    | 'elif' '(' expression ')' scope # ElifStatement
    | 'else' scope # ElseStatement
    | 'for' '(' symbolBinding 'in' expression ')' scope # ForStatement
    | 'while' '(' expression ')' scope # WhileStatement
    | 'repeat' scope # RepeatStatement
    | 'until' '(' expression ')' (';' | 'fixup' scope) # UntilStatement
    | 'within' scope # WithinStatement
    | 'apply' scope # ApplyStatement
    | 'using' '(' symbolBinding '=' qubitInitializer ')' scope # UsingStatement
    | 'borrowing' '(' symbolBinding '=' qubitInitializer ')' scope # BorrowingStatement
    ;

scope : BraceLeft statement* BraceRight;

symbolBinding
    : '_' # DiscardSymbol
    | Identifier # SymbolName
    | '(' (symbolBinding (',' symbolBinding)* ','?)? ')' # SymbolTuple
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
    | qualifiedName ('<' (type (',' type)* ','?)? '>')? # IdentifierExpression
    | IntegerLiteral # IntegerExpression
    | BigIntegerLiteral # BigIntegerExpression
    | DoubleLiteral # DoubleExpression
    | DoubleQuote stringContent* StringDoubleQuote # StringExpression
    | DollarQuote interpStringContent* InterpDoubleQuote # InterpStringExpression
    | boolLiteral # BoolExpression
    | resultLiteral # ResultExpression
    | pauliLiteral # PauliExpression
    | '(' (expression (',' expression)* ','?)? ')' # TupleExpression
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
    | expression ('+' | '-') expression # AddExpression
    | expression ('>>>' | '<<<') expression # ShiftExpression
    | expression ('>' | '<' | '>=' | '<=') expression # CompareExpression
    | expression ('==' | '!=') expression # EqualsExpression
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
    | expression 'w/' expression '<-' expression # UpdateExpression
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
