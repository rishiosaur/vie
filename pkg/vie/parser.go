package vie

import (
	"fmt"
	"strconv"
)

type Parser struct {
	lexer        *Lexer
	currentToken Token
	peekToken    Token

	errors []string

	prefixParsers map[TokenType]prefixParser
	infixParsers  map[TokenType]infixParser
}

type (
	prefixParser func() Expression
	infixParser  func(Expression) Expression
)

const (
	_ int = iota
	LOWEST
	PrecedenceAND
	PrecedenceOR
	EQUALS      // ==
	LESSGREATER // > or <

	SUM     //+
	PRODUCT //*
	PREFIX  //-Xor!X
	CALL    // myFunction(X)
	INDEX
	DOT
	FUNCTION
	MATCH
)

var precedences = map[TokenType]int{
	EQ:          EQUALS,
	NOT_EQ:      EQUALS,
	LT:          LESSGREATER,
	GT:          LESSGREATER,
	LTEQ:        LESSGREATER,
	GTEQ:        LESSGREATER,
	OR:          PrecedenceOR,
	AND:         PrecedenceAND,
	PLUS:        SUM,
	MINUS:       SUM,
	ASTERISK:    PRODUCT,
	SLASH:       PRODUCT,
	LPAREN:      CALL,
	LBRACKET:    INDEX,
	PERIOD:      DOT,
	ARROW:       FUNCTION,
	DOUBLECOLON: MATCH,
}

func NewParser(lexer *Lexer) *Parser {
	p := Parser{
		lexer:  lexer,
		errors: []string{},
	}

	p.consumeToken()
	p.consumeToken()

	p.prefixParsers = make(map[TokenType]prefixParser)
	p.infixParsers = make(map[TokenType]infixParser)

	p.registerPrefixFunction(TRUE, p.parseBoolean)
	p.registerPrefixFunction(FALSE, p.parseBoolean)
	p.registerPrefixFunction(INT, p.parseInt)
	p.registerPrefixFunction(FLOAT, p.parseFloat)
	p.registerPrefixFunction(STRING, p.parseString)
	p.registerPrefixFunction(IDENT, p.parseIdent)

	p.registerPrefixFunction(MINUS, p.parseGenericPrefix)
	p.registerPrefixFunction(BANG, p.parseGenericPrefix)
	p.registerPrefixFunction(PLUS, p.parseGenericPrefix)
	p.registerPrefixFunction(INCREMENT, p.parseGenericPrefix)
	p.registerPrefixFunction(DECREMENT, p.parseGenericPrefix)
	p.registerPrefixFunction(AMPERSAND, p.parseGenericPrefix)

	p.registerPrefixFunction(LPAREN, p.parseParenthesisExpr)

	p.registerInfixFunction(PLUS, p.parseGenericInfix)
	p.registerInfixFunction(MINUS, p.parseGenericInfix)
	p.registerInfixFunction(ASTERISK, p.parseGenericInfix)
	p.registerInfixFunction(SLASH, p.parseGenericInfix)
	p.registerInfixFunction(LT, p.parseGenericInfix)
	p.registerInfixFunction(GT, p.parseGenericInfix)
	p.registerInfixFunction(GTEQ, p.parseGenericInfix)
	p.registerInfixFunction(LTEQ, p.parseGenericInfix)
	p.registerInfixFunction(AND, p.parseGenericInfix)
	p.registerInfixFunction(OR, p.parseGenericInfix)
	p.registerInfixFunction(EQ, p.parseGenericInfix)
	p.registerInfixFunction(NOT_EQ, p.parseGenericInfix)
	p.registerInfixFunction(PERIOD, p.parseGenericInfix)
	return &p
}

func (p *Parser) Errors() []string {
	return p.errors
}

//
// UTILITY/PRECEDENCES
//

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}

	return LOWEST
}

func (p *Parser) currentPrecedence() int {
	if p, ok := precedences[p.currentToken.Type]; ok {
		return p
	}

	return LOWEST
}

//
// UTILITY/TOKENS
//

func (p *Parser) currentTokenIs(tok TokenType) bool {
	return p.currentToken.Type == tok
}

func (p *Parser) peekTokenIs(tok TokenType) bool {
	return p.peekToken.Type == tok
}

func (p *Parser) consumeToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

func (p *Parser) expectPeek(tok TokenType) bool {
	if p.peekTokenIs(tok) {
		p.consumeToken()
		return true
	}

	p.addPeekError(tok)
	return false

}

func (p *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	for p.currentToken.Type != EOF {
		_statement := p.parseStatement()

		if _statement != nil {
			program.Statements = append(program.Statements, _statement)
		}

		p.consumeToken()
	}

	return program

}

//
// UTILITY/ERRORS
//

func (p *Parser) addPeekError(tok TokenType) {
	msg := fmt.Sprintf("Expected next token to be %s, got %s instead", tok, p.peekToken)
	p.errors = append(p.errors, msg)
}

//
// UTILITY/REGISTRATION
//

func (p *Parser) registerPrefixFunction(tokenType TokenType, fn prefixParser) {
	p.prefixParsers[tokenType] = fn
}

func (p *Parser) registerInfixFunction(tokenType TokenType, fn infixParser) {
	p.infixParsers[tokenType] = fn
}

// Parsing Expressions
func (p *Parser) parseExpression(precedence int) Expression {

	prefix := p.prefixParsers[p.currentToken.Type]

	if prefix == nil {
		// p.noPrefixParseFnError(p.currentToken.Type)
		return nil
	}

	left := prefix()

	// fmt.Println("ipoj")
	// fmt.Println(p.peekPrecedence())
	for !p.peekTokenIs(SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParsers[p.peekToken.Type]
		if infix == nil {
			return left
		}

		p.consumeToken()

		left = infix(left)
	}

	return left

}

func (p *Parser) parseGenericPrefix() Expression {
	expression := &PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}

	p.consumeToken()
	expression.Right = p.parseExpression(LOWEST)
	return expression
}

func (p *Parser) parseParenthesisExpr() Expression {
	p.consumeToken()

	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseBoolean() Expression {
	return &BooleanLiteral{Token: p.currentToken, Value: p.currentTokenIs(TRUE)}
}

func (p *Parser) parseFloat() Expression {
	f, _ := strconv.ParseFloat(p.currentToken.Literal, 64)
	return &FloatLiteral{Token: p.currentToken, Value: f}
}

func (p *Parser) parseInt() Expression {
	f, _ := strconv.ParseInt(p.currentToken.Literal, 10, 64)
	return &IntegerLiteral{Token: p.currentToken, Value: f}
}

func (p *Parser) parseString() Expression {
	return &StringLiteral{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseIdent() Expression {
	return &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseGenericInfix(left Expression) Expression {
	// defer untrace(trace("parseInfixExpression:" + p.currentToken.Literal))

	expression := &InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}

	precedence := p.currentPrecedence()
	p.consumeToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

// Parsing statements

func (p *Parser) parseStatement() Statement {
	switch p.currentToken.Type {
	case IDENT:
		switch p.peekToken.Type {
		case DEFINE:
			return p.parseDefinition()
		case UPDATE:
			return p.parseUpdate()
		default:
			return p.parseExpressionStatement()
		}
	case RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	stmt := &ExpressionStatement{Token: p.currentToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(SEMICOLON) {
		p.consumeToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	stmt := &ReturnStatement{Token: p.currentToken}

	p.consumeToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	for !p.currentTokenIs(SEMICOLON) {
		p.consumeToken()
	}

	return stmt
}

func (p *Parser) parseDefinition() *DefinitionStatement {
	stmt := &DefinitionStatement{Name: &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}}
	if !p.expectPeek(DEFINE) {
		return nil
	}

	stmt.Token = p.currentToken
	p.consumeToken()

	stmt.Value = p.parseExpression(LOWEST)

	if p.peekTokenIs(SEMICOLON) {
		p.consumeToken()
	}

	return stmt

}

func (p *Parser) parseUpdate() *DefinitionStatement {
	stmt := &DefinitionStatement{Name: &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}}
	if !p.expectPeek(DEFINE) {
		return nil
	}

	stmt.Token = p.currentToken
	p.consumeToken()
	stmt.Value = p.parseExpression(LOWEST)

	if p.peekTokenIs(SEMICOLON) {
		p.consumeToken()
	}

	return stmt

}
