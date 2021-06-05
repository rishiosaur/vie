package vie

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"
	// Identifiers + literals
	IDENT = "IDENT" // add, foobar, x, y, ...
	INT   = "INT"   // 1343456
	FLOAT = "FLOAT"
	// Operators

	BANG        = "!"
	UNDERSCORE  = "_"
	AMPERSAND   = "&"
	BAR         = "|"
	PLUS        = "+"
	MINUS       = "-"
	ASTERISK    = "*"
	SLASH       = "/"
	LT          = "<"
	GT          = ">"
	EQ          = "=="
	NOT_EQ      = "!="
	ARROW       = "->"
	DEFINE      = ":="
	UPDATE      = "="
	DOUBLECOLON = "::"
	LTEQ        = "<="
	GTEQ        = ">="
	INCREMENT   = "++"
	DECREMENT   = "--"
	MINUSEQ     = "-="
	PLUSEQ      = "+="
	MULEQ       = "*="
	SLASHEQ     = "/="
	AND         = "&&"
	OR          = "||"

	// Delimiters
	PERIOD    = "."
	COMMA     = ","
	SEMICOLON = ";"
	LPAREN    = "("
	RPAREN    = ")"
	LBRACE    = "{"
	RBRACE    = "}"
	LBRACKET  = "["
	RBRACKET  = "]"
	COLON     = ":"

	// Primitives
	TRUE   = "TRUE"
	FALSE  = "FALSE"
	IF     = "IF"
	ELSE   = "ELSE"
	RETURN = "RETURN"
	STRING = "STRING"
)

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

var keywords = map[string]TokenType{
	"true":   TRUE,
	"false":  FALSE,
	"return": RETURN,
	"equals": EQ,
	"not":    NOT_EQ,
	"is":     DEFINE,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
