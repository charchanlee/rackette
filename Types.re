// Example data: "(+ 1 2)", "(lambda (x) (+ 2 x))"" 
type rawProgram = string;

/* Example data: NumberC(4), SymbolC("+"), SymbolC("/")
                ListC([SymbolC("+"), NumberC(4), NumberC(5)])*/
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* Example data: ListC([SymbolC("+"), NumberC(4), NumberC(1)]), 
                 [ListC([SymbolC("*"), NumberC(0), (NumberC(4))])]
                 [SymbolC("+"), NumberC(4), NumberC(1),
                  ListC([SymbolC("*"), NumberC(0), (NumberC(4))])]
*/
type concreteProgram = list(concreteProgramPiece);
/* example Data: [ListC([SymbolC("+"), NumberC(4), NumberC(1)]),
                 [ListC([SymbolC("*"), NumberC(0), (NumberC(4))])]
                 [SymbolC("+"), NumberC(4), NumberC(1),
                  ListC([SymbolC("*"), NumberC(0), (NumberC(4))])]]
*/

/* a Rackette name 
Example data: Name("input"), Name("proc-subtract"), Name("minus") */
type name =
  | Name(string);

/* a Rackette expression 
Example data: NumE(4), BoolE(true), EmptyE, AndE((4 > x), (2 == y)) 
OrE(BoolE(true), BoolE(false)), IfE(BoolE(true), NumE(10), NumE(11)), 
(CondE([(true) (NumE(10))])), LambdaE([Name("s")], NumE(10)), 
LetE([(Name("super"), NumE("10"))], BoolE(true)), ApplicationE([NumE(10)])
*/
type expression =
  | NumE(int) 
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(expression, expression, expression)
  | CondE(list((expression, expression)))
  | LambdaE(list(name), expression)
  | LetE(list((name, expression)), expression)
  | ApplicationE(list(expression));

/* a Rackette definition 
Example data: (Name("four"), NumE(4)), 
              (Name("or"), OrE(BoolE(true), BoolE(false)))
              (Name("empty"), EmptyE)  */
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 either a definition or an expression 
 Example Data: Definition(Name("four"), NumE(4))
               Expression(NumE(4))
               Expression(EmptyE)
               Expression(BoolE(true)) */

type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program -
 any number of pieces 
 Example Data: [Definition(Name("four"), NumE(4)), Expression(NumE(9))]
 [Definition(Name("bool"), BoolE(true))], [NumE(19)] */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression 
Example data: NumV(4), ListV([NumV(1), NumV(2)]), BoolV(true) */
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(string, list(value) => value)
  | ClosureV(list(name), expression, environment)
  and environment = (list(binding))
  and binding = (name, value);