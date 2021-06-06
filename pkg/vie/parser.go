package vie

import (
	"fmt"
	"strconv"

	"github.com/niemeyer/pretty"
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
	p.registerPrefixFunction(LBRACKET, p.parseArray)
	p.registerPrefixFunction(BAR, p.parseFunction)
	p.registerPrefixFunction(LBRACE, p.parseMap)

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
	p.registerInfixFunction(PERIOD, p.parseAccess)
	p.registerInfixFunction(DOUBLECOLON, p.parseMatch)

	p.registerInfixFunction(LPAREN, p.parseCall)
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

// func

// func flattenPairs(pairs []interface{}) []interface{} {
// 	var p []interface{}
// 	p = append(p, pairs[0])

// }

func (p *Parser) parseMap() Expression {
	mapLit := &MapLiteral{Token: p.currentToken}
	mapLit.Pairs = make(map[Expression]Expression)

	for !p.peekTokenIs(RBRACE) {
		p.consumeToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(COLON) {
			return nil
		}

		p.consumeToken()

		value := p.parseExpression(LOWEST)

		mapLit.Pairs[key] = value

		if !p.peekTokenIs(RBRACE) && !p.expectPeek(COMMA) {
			return nil
		}

	}

	if !p.expectPeek(RBRACE) {
		return nil
	}

	return mapLit

}

func (p *Parser) parseAccess(left Expression) Expression {

	switch l := left.(type) {
	case *Identifier:
		expr := &Identifier{Token: l.Token, Value: l.Value}

		for {
			p.consumeToken()

			var e Expression
			switch p.currentToken.Type {
			case LPAREN:
				e = p.parseParenthesisExpr()
			default:
				e = p.parseExpression(LOWEST)
			}
			pretty.Println(e)

			switch e := e.(type) {
			case *Identifier:
				expr.Value = append(expr.Value, e.Value...)
			case *FloatLiteral:
				expr.Value = append(expr.Value, e.Value)
			case *IntegerLiteral:
				expr.Value = append(expr.Value, e.Value)
			case *StringLiteral:
				expr.Value = append(expr.Value, e.Value)
			default:
				expr.Value = append(expr.Value, e)

			}

			if p.peekTokenIs(PERIOD) {
				p.consumeToken()
			} else if p.currentTokenIs(PERIOD) {
				continue
			} else {
				break
			}
		}

		// pretty.Println(p.peekToken)

		return expr

	default:
		return nil
	}
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

	pretty.Print(exp)

	return exp
}

func (p *Parser) parseArray() Expression {
	array := &ArrayLiteral{Token: p.currentToken}
	array.Elements = p.parseExpressionList(RBRACKET)
	return array
}

func (p *Parser) parseExpressionList(delimiter TokenType) []Expression {
	exps := []Expression{}

	if p.peekTokenIs(delimiter) {
		p.consumeToken()
		return exps
	}

	p.consumeToken()

	exps = append(exps, p.parseExpression(LOWEST))
	for p.peekTokenIs(COMMA) {
		p.consumeToken()
		p.consumeToken()
		exps = append(exps, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(delimiter) {
		return nil
	}

	return exps
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
	var value []interface{}
	value = append(value, p.currentToken.Literal)
	return &Identifier{Token: p.currentToken, Value: value}
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

func (p *Parser) parseMatch(left Expression) Expression {

	expression := &MatchExpression{
		Token:      p.currentToken,
		Expression: left,
		blocks:     make(map[Expression]*BlockStatement),
	}

	if !p.expectPeek(LBRACE) {
		return nil
	}

	p.consumeToken()

	for {
		matcher := p.parseExpression(LOWEST)

		if !p.expectPeek(ARROW) {
			return nil
		}

		token := p.currentToken

		p.consumeToken()

		switch p.currentToken.Type {
		case LPAREN:
			expr := p.parseExpression(LOWEST)
			expression.blocks[matcher] = &BlockStatement{Token: token, Statements: []Statement{
				&ReturnStatement{Token: p.currentToken, ReturnValue: expr},
			}}
		case LBRACE:
			block := p.parseBlockStatement()
			expression.blocks[matcher] = block

		default:
			expr := p.parseExpression(LOWEST)
			expression.blocks[matcher] = &BlockStatement{Token: token, Statements: []Statement{
				&ReturnStatement{Token: p.currentToken, ReturnValue: expr},
			}}
		}

		if p.peekTokenIs(RBRACE) {
			p.consumeToken()
			break
		} else {
			p.consumeToken()
		}

	}

	return expression
}

func (p *Parser) parseFunction() Expression {
	f := &FunctionLiteral{Token: p.currentToken}
	f.Parameters = p.parseFunctionParameters()

	f.Body = &BlockStatement{Token: p.currentToken, Statements: []Statement{}}
	if !p.expectPeek(ARROW) {
		return nil
	}

	switch p.peekToken.Type {
	case LPAREN:

		p.consumeToken()
		p.consumeToken()

		expr := p.parseExpression(LOWEST)
		f.Body.Statements = append(f.Body.Statements, &ReturnStatement{Token: p.currentToken, ReturnValue: expr})
	case LBRACE:
		p.consumeToken()
		f.Body = p.parseBlockStatement()

	default:
		// p.consumeToken()
		p.consumeToken()
		expr := p.parseExpression(LOWEST)

		f.Body.Statements = append(f.Body.Statements, &ReturnStatement{Token: p.currentToken, ReturnValue: expr})
	}

	return f
}

func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{Token: p.currentToken}
	block.Statements = []Statement{}

	p.consumeToken()

	for !p.currentTokenIs(RBRACE) && !p.currentTokenIs(EOF) {
		stmt := p.parseStatement()

		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.consumeToken()
	}

	return block
}

func (p *Parser) parseFunctionParameters() []*Identifier {
	idents := []*Identifier{}

	if p.peekTokenIs(BAR) {
		p.consumeToken()
		return idents
	}

	p.consumeToken()

	var value []interface{}
	value = append(value, p.currentToken.Literal)
	ident := &Identifier{Token: p.currentToken, Value: value}
	idents = append(idents, ident)

	for p.peekTokenIs(COMMA) {
		p.consumeToken()
		p.consumeToken()
		var value []interface{}
		value = append(value, p.currentToken.Literal)
		ident := &Identifier{Token: p.currentToken, Value: value}
		idents = append(idents, ident)

	}

	if !p.expectPeek(BAR) {
		return nil
	}

	return idents
}

func (p *Parser) parseCall(ident Expression) Expression {
	exp := &CallExpression{Token: p.currentToken, Function: ident}
	exp.Arguments = p.parseExpressionList(RPAREN)
	return exp
}

// Parsing statements

func (p *Parser) parseStatement() Statement {
	switch p.currentToken.Type {
	case IDENT:
		expr := p.parseExpression(LOWEST)
		var idents []interface{}
		switch e := expr.(type) {
		case *Identifier:
			idents = e.Value
		default:
			return nil

		}
		switch p.peekToken.Type {
		case DEFINE:
			return p.parseDefinition(idents)
		case UPDATE:
			return p.parseUpdate(idents)
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

	return stmt
}

func (p *Parser) parseDefinition(values []interface{}) *DefinitionStatement {
	stmt := &DefinitionStatement{Name: &Identifier{Token: p.currentToken, Value: values}}
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

func (p *Parser) parseUpdate(values []interface{}) *DefinitionStatement {
	stmt := &DefinitionStatement{Name: &Identifier{Token: p.currentToken, Value: values}}
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
