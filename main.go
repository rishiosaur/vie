package main

import (
	"fmt"

	"github.com/rishiosaur/vie/pkg/vie"
)

func main() {
	str := "33.344346a[]true"

	l := vie.NewLexer(str)
	fmt.Printf("%+v\n", l.NextToken())
	fmt.Printf("%+v\n", l.NextToken())
	fmt.Printf("%+v\n", l.NextToken())
	fmt.Printf("%+v\n", l.NextToken())
	fmt.Printf("%+v\n", l.NextToken())
	fmt.Printf("%+v\n", l.NextToken())

}
