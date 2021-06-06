package vie

import (
	"fmt"

	"github.com/kr/pretty"
)

func newError(format string, a ...interface{}) *Error {
	return &Error{Message: fmt.Sprintf(format, a...)}
}

func isError(obj Object) bool {
	if obj != nil {
		return obj.Type() == ERROR_OBJ
	}

	return false
}

var (
	OTRUE  = &Boolean{Value: true}
	OFALSE = &Boolean{Value: false}
	NULL   = &Null{}
)

func toBool(input bool) *Boolean {
	if input {
		return OTRUE
	}

	return OFALSE
}

func Eval(node Node, machine *Machine) Object {

	switch node := node.(type) {
	case *Program:
		return evalProgram(node, machine)

	case *ExpressionStatement:
		return Eval(node.Expression, machine)
	case *ReturnStatement:
		val := Eval(node.ReturnValue, machine)
		if isError(val) {
			return val
		}
		return &ReturnValue{Value: val}

	// case *BlockStatement:
	// 	return evalBlockStatement(node, machine)
	case *DefinitionStatement:
		// println("hi")
		val := Eval(node.Value, machine)
		pretty.Println(val)
		if isError(val) {
			return val
		}
		machine.Set(node.Name, val)
	// case *UpdateStatement:
	// 	val := Eval(node.Value, machine)
	// 	if isError(val) {
	// 		return val
	// 	}
	// 	machine.Update(node.Name.Value), val)

	case *FunctionLiteral:
		params := node.Parameters
		body := node.Body
		return &Function{Parameters: params, Machine: machine, Body: body}
	case *IntegerLiteral:
		return &Integer{Value: node.Value}
	case *StringLiteral:
		return &String{Value: node.Value}
	case *BooleanLiteral:
		return toBool(node.Value)
	case *Identifier:

		if val, ok := machine.Get(node); ok {
			return val
		}
	case *MapLiteral:
		mv := make(map[Object]Object)

		for k, v := range node.Pairs {
			key := Eval(k, machine)
			val := Eval(v, machine)

			mv[key] = val
		}

		return &Map{Value: mv}

		// if builtin, ok :=
		// return newError("identifier not found: " + node.Value)

	case *PrefixExpression:
		right := Eval(node.Right, machine)
		if isError(right) {
			return right
		}
		return evalPrefixExpression(node.Operator, right)
	default:
		pretty.Println(node)
		return newError("I literally have no clue wtf that is. RTFM pls.")
	}
	return nil
}

func evalProgram(program *Program, machine *Machine) Object {
	var result Object

	for _, statement := range program.Statements {
		result = Eval(statement, machine)
		switch result := result.(type) {
		case *ReturnValue:
			return result.Value
		case *Error:
			return result
		}
	}

	return result
}

func evalPrefixExpression(operator string, right Object) Object {
	switch operator {
	case "!":
		return evalNegationOpExpression(right)
	case "-":
		return evalMinusOpExpression(right)
	default:
		return newError("unknown operator: %s%s", operator, right.Type())
	}
}

func evalNegationOpExpression(right Object) Object {

	switch right {
	case OTRUE:
		return OFALSE
	case OFALSE:
		return OTRUE
	case NULL:
		return OTRUE
	default:
		return OFALSE
	}
}

func evalMinusOpExpression(right Object) Object {

	if right.Type() != INTEGER_OBJ {
		return newError("unknown operator: -%s", right.Type())
	}

	value := right.(*Integer).Value

	return &Integer{Value: -value}
}
