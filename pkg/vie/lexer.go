package vie

type Lexer struct {
	input           string
	position        int
	readingPosition int
	currentChar     byte
}

func NewLexer(input string) *Lexer {
	lexer := Lexer{input: input}
	lexer.consumeChar()
	return &lexer
}

func (lexer *Lexer) consumeChar() {
	if lexer.readingPosition >= len(lexer.input) {
		lexer.currentChar = 0
	} else {
		lexer.currentChar = lexer.input[lexer.readingPosition]
	}

	lexer.position = lexer.readingPosition
	lexer.readingPosition++
}

func (lexer *Lexer) peekCharacter() byte {
	if lexer.readingPosition >= len(lexer.input) {
		return 0
	} else {
		return lexer.input[lexer.readingPosition]
	}
}

func (lexer *Lexer) skipWhitespace() {
	for IsWhitespace(lexer.currentChar) {
		lexer.consumeChar()
	}
}

func (lexer *Lexer) NextToken() Token {
	var t Token
	singleCharToken := func(t TokenType) Token {
		return Token{
			Literal: string(lexer.currentChar),
			Type:    t,
		}
	}

	doubleCharToken := func(t TokenType) Token {
		character := lexer.currentChar
		lexer.consumeChar()
		return Token{Type: t, Literal: string(character) + string(lexer.currentChar)}
	}

	lexer.skipWhitespace()

	switch lexer.currentChar {
	case '(':
		t = singleCharToken(LPAREN)
	case '.':
		t = singleCharToken(PERIOD)
	case ')':
		t = singleCharToken(RPAREN)
	case '[':
		t = singleCharToken(LBRACKET)
	case ']':
		t = singleCharToken(RBRACKET)
	case ',':
		t = singleCharToken(COMMA)
	case '+':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(PLUSEQ)
		case '+':
			t = doubleCharToken(INCREMENT)
		default:
			t = singleCharToken(PLUS)
		}
	case '-':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(MINUSEQ)
		case '+':
			t = doubleCharToken(DECREMENT)
		case '>':
			t = doubleCharToken(ARROW)
		default:
			t = singleCharToken(MINUS)
		}
	case '*':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(MULEQ)
		default:
			t = singleCharToken(ASTERISK)
		}
	case '!':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(NOT_EQ)
		default:
			t = singleCharToken(BANG)
		}
	case '/':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(SLASHEQ)
		default:
			t = singleCharToken(SLASH)
		}
	case '>':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(GTEQ)
		default:
			t = singleCharToken(GT)
		}
	case '<':
		switch lexer.peekCharacter() {
		case '=':
			t = doubleCharToken(LTEQ)
		default:
			t = singleCharToken(LT)
		}
	case ':':
		switch lexer.peekCharacter() {
		case ':':
			t = doubleCharToken(DOUBLECOLON)
		case '=':
			t = doubleCharToken(DEFINE)
		default:
			t = singleCharToken(COLON)
		}

	case '&':
		switch lexer.peekCharacter() {
		case '&':
			t = doubleCharToken(AND)
		default:
			t = singleCharToken(AMPERSAND)
		}
	case '|':
		switch lexer.peekCharacter() {
		case '|':
			t = doubleCharToken(OR)
		default:
			t = singleCharToken(BAR)
		}
	case 0:
		t.Literal = ""
		t.Type = EOF
	case '\'', '"', '`':

		delimiter := lexer.currentChar
		position := lexer.position + 1
		for {
			lexer.consumeChar()

			if lexer.currentChar == delimiter || lexer.currentChar == 0 {
				break
			}
		}

		t.Type = STRING
		t.Literal = lexer.input[position:lexer.position]

	default:
		if IsLetter(lexer.currentChar) {
			t.Literal = lexer.consumeIdentifier()
			t.Type = LookupIdent(t.Literal)
			return t
		} else if IsDigit(lexer.currentChar) {
			ty, lit := lexer.consumeNumber()
			t.Literal = lit
			t.Type = ty
			return t
		} else {
			t = singleCharToken(ILLEGAL)

		}
	}
	lexer.consumeChar()

	return t

}

func (lexer *Lexer) consumeIdentifier() string {
	position := lexer.position
	for IsLetter(lexer.currentChar) {
		lexer.consumeChar()
	}
	return lexer.input[position:lexer.position]
}

func (lexer *Lexer) consumeNumber() (TokenType, string) {
	position := lexer.position
	var t TokenType = INT

	for IsDigit(lexer.currentChar) {
		lexer.consumeChar()
	}

	if string(lexer.currentChar) == "." {
		t = FLOAT
		lexer.consumeChar()
		for IsDigit(lexer.currentChar) {
			lexer.consumeChar()
		}
	}

	return t, lexer.input[position:lexer.position]
}
