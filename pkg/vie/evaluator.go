package vie

import "fmt"

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
	case *BlockStatement:
		return evalBlockStatement(node, machine)
	case *DefinitionStatement:
		val := Eval(node.Value, machine)
		if isError(val) {
			return val
		}
		machine.Set(node.Name.Value, val)
	case *UpdateStatement:
		val := Eval(node.Value, machine)
		if isError(val) {
			return val
		}
		machine.Update(node.Name.Value, val)
	default:
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
