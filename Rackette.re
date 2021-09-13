open CS17SetupRackette;
open Read.Reader;
open Types;

/* Data definitions: 
concreteProgramPiece: either a NumberC(), SymbolC(), or ListC(). The intent 
of this type is to sort into numbers, symbols, or lists, and these are grouped
between parentheses, this categorization narrows down what the interpreter 
should do with each piece. 

ListC(): a list of other concrete program pieces. A list is identified whenever
there is something between matching parentheses. 

concreteProgram: a list of concreteProgramPieces. The intent of this type is to 
act as an intermediary between the user input (string form of racket program, 
rawProgram), in which all parts of the rawProgram are sorted as either numbers, 
symbols, or a list of numbers, symbols, or lists. 

expression: a type of abstractProgramPiece that is unique as it is a representation
of a value, and needs to be evaluated.

  and expression: contains two expressions. When the AndE is evaluated, it is 
    true if both expressions inside are true. 
  or expression:contains two expressions. When the AndE is evaluated, it is 
    true if one expression inside is true.  
  if expression: contains three expressions, a predicate, true case, and false 
    case. If the predicate is true, the true case will later be evaluated, 
    else the false case will later be evlauted. 
  cond expression: contains a list of tuples of expressions: (predicate, then ), 
    in which in evaluating, we look at all the predicates in order and evaluate
    the then expression that corresponds to the first predicate that evaluates to 
    true. 
  lambda expression: an anonymous function expression that contains a list 
    of all the involved formal arguments followed by an expression indicating
    what to do with those arguments. 
  let expression: contains a list of definitions followed by an expression
   indicating that something must be evaluated under an environment where 
   those definitios are added to a temporary environment. 
  application expression: a list of expressions in which the first is a procedure
  and the rest are arguments that the procedure must be applied to. 

definition: a tuple including a name an expression. A definition must be added
into an evironment, which is essentially a dictionary. In order to evaluate 
other expressions correctly, we must have certain definitions to lookup. 


abstractProgramPiece: either a definition or expression. By sorting into these 
two groups, we know whether we should either add the definition or evaluate the
expression. concreteProgramPieces are converted into corresponding 
abstractProgramPieces.

abstractProgram: a list of abstractProgramPieces. This is another intermediary 
form, converted from a concreteProgram. 

value: a value is the last intermediary form. abstractProgramPieces are converted
into values, in which it has a constant value (number, boolean), or at least 
a constant value given constant input expressions (applies to closures and 
builtins). 

binding: a binding is slightly different from a definition, but it is also a tuple. 
The difference is that it is a (name, value) tuple. 

environment: an environment has a list of bindings, in which can be useful for 
looking up names and finding the corresponding values so that expressions can 
be evaluated. 
*/

/* PLUS
 * Input:  l1, a list of values.  This should contain two NumV's and no more, and 
          and nothing of any other type, but we check for that with error testing
 * Output: a NumV containing the sum of the two elements in l1. */

let plus: list(value) => value = l1 =>
switch (l1) {
  |[NumV(n1), NumV(n2)] => NumV(n1 + n2)
  |_ => failwith ("invalid addition inputs, needs 2")
}


/* MINUS
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a NumV containing the difference between the two nums in l1, subtracted 
        in input list order.
*/

let minus: list(value) => value = l1 =>
switch (l1) {
  |[NumV(n1), NumV(n2)] => NumV(n1 - n2)
  |_ => failwith ("invalid subtraction inputs, needs 2")
}

/* MULTIPLY
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a NumV containing the product of the two nums in l1.  
*/

let multiply: list(value) => value = l1 =>
switch (l1) {
  |[NumV(n1), NumV(n2)] => NumV(n1 * n2)
  |_ => failwith ("invalid multiplication inputs, needs 2")
}


/* DIVIDE 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a NumV containing the quotient of the two nums in l1, divided in
        input list order 
*/

let divide: list(value) => value = l1 =>
switch (l1) {
  |[NumV(n1), NumV(n2)] when n2 != 0 => NumV(n1 / n2)
  |_ => failwith ("invalid division inputs, needs 2 elements of NumV() and 
  cannot divide by zero")
}


/* REMAINDER 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a NumV containing the remainder of num 1 divided by num2 
*/

let remainder: list(value) => value = l1 =>
switch (l1) {
  |[NumV(n1), NumV(n2)] when n2 != 0 => NumV(n1 - ((n1 / n2) * n2))
  |_ => failwith ("invalid remainder inputs, needs 2 elements of NumV(), cannot
  divide by zero")
}

/* EQUALSNUMP 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a BoolV() with true if the two nums are equal and false otherwise 
*/

let equalsNumP: list(value) => value = l1 =>
switch (l1) {
    |[NumV(n1), NumV(n2)] => BoolV(n1 == n2)
  |_ => failwith ("invalid equalNumP inputs, needs 2")
}

/* LESSTHANP 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a BoolV() with true if 1st element < 2nd element, and false otherwise 
*/

let lessThanP: list(value) => value = l1 =>
switch (l1) {
    |[NumV(n1), NumV(n2)] => BoolV(n1 < n2)
  |_ => failwith ("invalid < inputs, needs 2")
};


/* GREATERTHANP 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a BoolV() with true if 1st element > 2nd element, and false otherwise 
*/

let greaterThanP: list(value) => value = l1 =>
switch (l1) {
    |[NumV(n1), NumV(n2)] => BoolV(n1 > n2)
  |_ => failwith ("invalid > inputs, needs 2")
};


/* LESSTHANOREQUALP 
Input: l1, a list of values.  This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a BoolV() with true if 1st element <=  2nd element, and false otherwise 
*/


let lessOrEqualP: list(value) => value = l1 =>
switch (l1) {
    |[NumV(n1), NumV(n2)] => BoolV(n1 <= n2)
  |_ => failwith ("invalid <= inputs, needs 2")
}


/* GREATEROREQUAL 
Input: l1, a list of values. This should contain two NumV's and no more, and 
       and nothing of any other type, but we check for that with error testing
Output: a BoolV() with true if 1st element >=  2nd element, and false otherwise 
*/

let greaterOrEqualP: list(value) => value = l1 =>
switch (l1) {
    |[NumV(n1), NumV(n2)] => BoolV(n1 >= n2)
  |_ => failwith ("invalid => inputs, needs 2")
}

/* EQUALP 
Input: l1, a list of values. This should contain two values of the same type 
      and no more, but we check for that with error testing
Output: a BoolV() with true if elements are equal, and false otherwise 
*/

let equalP: list(value) => value = l1 =>
switch (l1) {
    | [NumV(n1), NumV(n2)] => BoolV(n1 == n2)
    | [BoolV(b1), BoolV(b2)] => BoolV(b1 == b2)
    | [ListV(l1), ListV(l2)] => BoolV(l1 == l2)
    |_ => failwith ("invalid equal? inputs, needs 2")
};



/* NUMBERP 
Input: v1, a value 
Output: a BoolV() with true if the value is of type NumV(), and false otherwise 
*/

let numberP: list(value) => value = v1 =>
switch (v1) {
  | [NumV(_)] => BoolV(true)
  | [_] => BoolV(false)
};


/* ZEROP 
Input: v1, a value of type NumV(), which is checked with error 
Output: a BoolV() with true if the value = 0, and false otherwise 
*/

let zeroP: list(value) => value = v1 =>
switch (v1) {
  | [NumV(0)] => BoolV(true)
  | [NumV(_)] => BoolV(false)
  | [_] => failwith("invalid zeroP input, needs a NumV")
}




/* CONSlIST
Input: input, a list of values with two elements, the first is of type NumV()
      and the second is of type ListV(), which is checked with error
Output: a new list where the NumV() is added to the front of the old ListV() */ 


let consList: list(value) => value = lst =>
switch (lst) {
  | [NumV(n), ListV(ls)] => ListV([NumV(n), ...ls])
  | _ => failwith("invalid cons inputs, needs a NumV and a ListV")
};

/* FIRST 
Input: lst, list of values of type ListV()
Output: the first element in the lst 
*/

let first: list(value) => value = lst => 
switch (lst) {
  | [ListV([hd,..._])] => hd 
  | [_] => failwith("invalid first input, must be a nonempty ListV")
}


/* REST 
Input: lst, list of values of type ListV()
Output: the same list of values, type ListV(), without the first element
*/

let rest: list(value) => value = lst => 
switch (lst) {
  | [ListV([_,...tl])] => ListV(tl)
  | [_] => failwith("invalid rest input, must be a ListV")
}


/* EMPTYP 
Input: l1, a list of values of type ListV(), checked with error
Output: a BoolV() with if the list is empty, and false otherwise 
*/

let emptyP: list(value) => value = lst => 
switch (lst) {
  | [ListV([])] => BoolV(true)
  | [ListV([_,..._])] => BoolV(false)
  | [_] => failwith("invalid emptyP input, must be a ListV")
}

/* CONSP 
Input: l1, a list of values of type ListV(), checked with error
Output: a BoolV() with if the list is not empty, and false otherwise 
*/

let consP: list(value) => value = lst => 
switch (lst) {
  | [ListV([])] => BoolV(false)
  | [ListV([_,..._])] => BoolV(true)
  | _ => failwith("invalid consP input, must be a ListV")
}


/* NOT 
Input: a value of type BoolV()
Output: false if true, true if false 
*/

let notBI: list(value) => value = input => 
switch (input) {
  | [BoolV(false)] => BoolV(true)
  | [BoolV(true)] => BoolV(false)
  | _ => failwith("invalid not input, must be a BoolV")
}



let initialTle: environment = [
(Name("+"), (BuiltinV("<builtin-plus>", plus))),
(Name("-"), (BuiltinV("<builtin-minus>", minus))), 
(Name("*"), (BuiltinV("<builtin-mult>", multiply))),
(Name("/"), (BuiltinV("<builtin-divide>", divide))),
(Name("remainder"), (BuiltinV("<builtin-remainder>", remainder))),
(Name("="), (BuiltinV("<builtin-equalsNum?>", equalsNumP))),
(Name("<"), (BuiltinV("<builtin-lessThan?>", lessThanP))),
(Name(">"), (BuiltinV("<builtin-greaterThan?>", greaterThanP))),
(Name("<="), (BuiltinV("<builtin-lessOrEqual?>", lessOrEqualP))),
(Name(">="), (BuiltinV("<builtin-greaterOrEqual?>", greaterOrEqualP))),
(Name("equal?"), (BuiltinV("<builtin-equal?>", equalP))),
(Name("number?"), (BuiltinV("<builtin-number?>", numberP))),
(Name("zero?"), (BuiltinV("<builtin-zero?>", zeroP))),
(Name("cons"), (BuiltinV("<builtin-cons>", consList))),
(Name("first"), (BuiltinV("<builtin-first>", first))),
(Name("rest"), (BuiltinV("<builtin-rest>", rest))),
(Name("empty?"), (BuiltinV("<builtin-empty?>", emptyP))),
(Name("cons?"), (BuiltinV("<builtin-cons?>", consP))),
(Name("not"), (BuiltinV("<builtin-not>", notBI))),
];




/*

Recursion Diargams: 
OI: ListC([SymbolC("and"), SymbolC("Charlotte"), SymbolC("Emily")])
RI: SymbolC("Charlotte")
RO: NameE("Charlotte")
OO: AndE(NameE("Charlotte"), NameE("Emily"))

OI: ListC([SymbolC("or"), SymbolC("x"), SymbolC("y")])
RI: SymbolC("x")
RO: NameE("x")
OO: OrE(NameE("x"), NameE("y"))

Input: input, a concreteProgramPiece
Output: the result of converting the concreteProgramPiece into a type expression 

*/
let rec parseExpression: concreteProgramPiece => expression =
  input => 
  switch(input) {
    | NumberC(n)=> NumE(n)
        | SymbolC(s) => switch (s) {
                      | "true" => BoolE(true)
                      | "false" => BoolE(false)
                      | "empty" => EmptyE
                      | _ => NameE(Name(s))
                      };
                    
    | ListC(l) => switch (l) {
                    | [SymbolC("and"), a, b] => 
                      AndE(parseExpression(a), parseExpression(b)) 
                    | [SymbolC("or"), a, b] => 
                      OrE(parseExpression(a), parseExpression(b))
                    | [SymbolC("if"), pred, trueCase, falseCase] => 
                      IfE(parseExpression(pred), 
                        parseExpression(trueCase), 
                        parseExpression(falseCase))
                    /* Mapping an anonymous function to extract from the
                    constructor and convert types*/
                    | [SymbolC("cond"), ListC([pred, act]), ...tail] => 
                      CondE(List.map((fun| ListC([p, a]) =>
                             (parseExpression(p), parseExpression(a))), 
                                      [ListC([pred, act]), ...tail]))
                    | [SymbolC("lambda"), ListC(arguments), body] => 
                      LambdaE(List.map((fun| SymbolC(x) => Name(x)), arguments),
                               parseExpression(body))
                    | [SymbolC("let"), ListC([ListC([nm, va]), ...tl]), ListC(body)] => 
                    LetE(List.map((fun| ListC([SymbolC(n), v]) => 
                      (Name(n), parseExpression(v))), [ListC([nm, va]), ...tl]),
                         parseExpression(ListC(body)))
                    | _ => ApplicationE(List.map(parseExpression, l))
                      };
  };
  
  
/* parseDefinition

Input: a concreteProgramPiece, input 
Output: if the input  is a ListC() type which begins with the 
SymbolC("define"), input must be converted into a definition, in which the 
second element of the input list is converted from SymbolC() into a name, and 
the remaining elements are converted into the corresponding expression portion 
of the definition. 

 */
let parseDefinition: concreteProgramPiece => definition =
  input => 
  switch(input){
    | ListC([SymbolC("define"), SymbolC(a), NumberC(n)]) => (Name(a), NumE(n))
    | ListC([SymbolC("define"), SymbolC(a), ListC(contents)]) => 
      (Name(a), parseExpression(ListC(contents)))
    |_ => failwith("Definition syntax error")
  }
checkError(() => parseDefinition(NumberC(5)), "Definition syntax error"); 

/* parsePiece

Input: a concreteProgramPiece, input 
Output: the input is sorted into either a Definition() or Expression(), a type of
abstractProgramPiece
*/
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* parse

Input: a concreteProgram, input 
Output: an abstractProgram, where every item in list(abstractProgramPiece)
corresponds to the conversion of every item in the input under the procedure 
"parsePiece", in order
*/
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* addBinding 
Data Definition: 
binding: defined in Types. re as (name, value)
environment: defined in Types.re as a list(binding)

Example Data: 
binding: (Name("Charlotte"), NumV(19))
environment: [(Name("Charlotte"), NumV(19))]

Input: a name, n, value, va, and environment, env
Output: a new environment in which the binding (n, va) is added to the list
*/

let addBinding: (name, value, environment) => (environment) =
(n, va, env) => {
    [(n, va), ...env]
    };

/* extendEnv 

Input: two environments, local and tle
Output: a new, larger environment in which the two environments are combined with 
List.append, but all the bindings in local come before the bindings in tle. 
*/

let extendEnv: (environment, environment) => environment =
(local, tle) => {
    List.append(local, tle)
}; 



/* lookup 

Recursion Diagrams: 
OI: Name("Sergio Perez")
[(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95)]
RI: Name("Sergio Perez") [(Name("Lewis Hamilton"), 95)]
RO: None 
OO: None
OI: Name("Max Verstappen")
[(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95)]
RI: Name("Max Verstappen") [(Name("Lewis Hamilton"), 95)]
RO: None 
OO: Some(10)

Input: a name, n, and evironment, env
Output: the corresponding value to n in the env as an option: Some(value), or 
None if it does not exist
*/

let rec lookup: (name, environment) => option(value) = 
(n, env) => switch (env) {
    | [(Name(id), va),...tl] => if (id == ((fun| Name(s) => s) (n))) {Some(va)} 
                                else {lookup(n, tl)}; 
    | [] => None
};
checkExpect(lookup(Name("+"), [(Name("+"), NumV(10))]), Some(NumV(10)), "tle");
checkExpect(lookup(Name("hi"), initialTle), None, "Not in tle");

let emptyEnv: environment = []; 

/* addMultBindings

Recursion Diagrams: 
OI: [(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95)], []
RI: [Name("Lewis Hamilton"), 95)], []
RO: [Name("Lewis Hamilton"), 95)] 
OO: [(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95)]

OI: [(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95)], 
[(Name("Sergio Perez"), 1)]
RI: [Name("Lewis Hamilton"), 95)], [(Name("Sergio Perez"), 1)]
RO: [(Name("Lewis Hamilton"), 95), Name("Sergio Perez"), 1)] 
OO: [(Name("Max Verstappen"), 10), (Name("Lewis Hamilton"), 95), 
Name("Sergio Perez"), 1)]

Input: a list of bindings, argumentList, and an environment, locEnv
Output: an environment in which all bindings in argumentList are added to locEnv
with the addBindings procedure

* eval

Recursion Diagrams: 

OI: tle, [(Name(a), BoolV(true)), (Name(b), BoolV(true))], AndE(a, b)
RI: tle, [(Name(a), BoolV(true)), (Name(b), BoolV(true))], NameE(a)
RO: BoolV(true)
OO: BoolV(true)

OI: tle, [(Name(a), BoolV(true)), (Name(b), BoolV(false))], AndE(a, b)
RI: tle, [(Name(a), BoolV(true)), (Name(b), BoolV(false))], NameE(a)
RO: BoolV(true)
OO: BoolV(false)

Input: two environments, tle and env, and an expression, expr, in which 
the expr is to be evaluated
Output: convert expr into type value, given the bindings in tle and env

*/

 let rec addMultBindings: (list(binding), environment) => environment 
= (argumentList, locEnv) =>
switch (argumentList) {
  | [(nm, va)] => addBinding(nm, va, locEnv)
  | [(nm, va), ...tl] => addBinding(nm, va, addMultBindings(tl, locEnv))
  | _ => failwith("addMultBindings")
}
and 
eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
  switch (expr) {
  | NumE(n) => NumV(n)
  | BoolE(true) => BoolV(true) 
  | BoolE(false) => BoolV(false)
  | EmptyE => ListV([])
  | NameE(name) => switch (lookup(name, extendEnv(env, tle))) {
                    | Some(v) => v
                    | None => failwith("Name is not bound")
                    | _ => failwith("name lookup fail")
                    }; 
  | AndE(a, b) => if (((eval(tle, env, a)) == BoolV(true)) 
                        && ((eval(tle, env, b) == BoolV(true))))
                  {BoolV(true)} else {BoolV(false)}; 
  | OrE(a, b) => if ((eval(tle, env, a) == BoolV(true)) 
                      || (eval(tle, env, b) == BoolV(true)))
                  {BoolV(true)} else {BoolV(false)}; 
  | IfE(pred, trueCase, falseCase) => if (eval(tle, env, pred) == BoolV(true))
                                      {eval(tle, env, trueCase)} 
                                      else {eval(tle, env, falseCase)}; 
  | CondE([(pred, res),...tail]) => if (eval(tle, env, pred) == BoolV(true))
                                    {eval(tle, env, res)} 
                                    else {eval(tle, env, CondE(tail))}; 
  | LambdaE(argumentList, body) => ClosureV(argumentList, body, env) 
  | LetE(argumentList, body) =>  /* Extract all the expressions from 
    the tuples in the argumentList and evaluate them to convert definitions
    into bindings */
    let argVals = List.map((fun| (nm, expr) => 
      (nm, eval(tle, env, expr))), argumentList)
    /* Evaluate the body in an environment where the let bindings exist
      temporarily*/
    eval(tle, extendEnv(addMultBindings(argVals, []), env), body);
  | ApplicationE([head,...tail]) => 
    /* actuals: evaluates the actual arguments to get values 
    pairUp: pairs up a formal and actual argument, which is later mapped to
    create a list of bindings */
    let actuals = List.map((fun| argX => eval(tle, env, argX)), tail);
    let pairUp: (name, value) => binding = (nm, expr) => (nm, expr); 
    switch (eval(tle, env, head)){
      | BuiltinV(str, proc) => proc(actuals) 
      | ClosureV(argList, body, env) => 
        let tempEnv = extendEnv(
          addMultBindings(List.map2(pairUp, argList, actuals), []),
          env);
        if (List.length(argList) == List.length(actuals)) 
          {eval(tle, tempEnv, body)}
          else {failwith("List of formals and actuals have different lengths")}
    };
  |_ => failwith("parseExpression fail") 
  }; 

/* addDefinition
Input: an environment, env, and a (name, expression), (id, expr)
Output: if the name is not already bound to a value in the env, then evaluate the
value of expr and add that binding, If the name is alread bound, failwith. 
Problem: we only lookup the name in the initialTle, but what if it's bound locally
 */
let addDefinition: (environment, (name, expression)) => environment = 
  (env, (id, expr)) => 
  if (lookup(id, initialTle) == None) 
    {addBinding(id, eval(initialTle, env, expr), initialTle)} 
   else {failwith("Definition is already bound")}; 

/* stringOfValue
Input: aValue, a value
Output: extracts the value from the constructor under the type value and turns 
it into a string representation
*/

let rec stringOfValue: value => string =
  aValue => 
  switch (aValue) {
    | NumV(n) => string_of_int(n)
    | BoolV(b) => string_of_bool(b)
      /* For lists, add parentheses and "cons" recursively*/
    | ListV([hd]) => "(cons " ++ stringOfValue(hd) ++ ")"
    | ListV([hd, ...tl]) => "(cons" ++ 
                            stringOfValue(hd) ++ stringOfValue(ListV(tl)) 
                            ++ ")"
    | BuiltinV(str, proc) => str
    | ClosureV(args, expr, env) => "<user defined procedure>"
    |_ => failwith("stringOfValue fail")
  }

/* process
Input: an abstractProgram, pieces 
Output: conversion of the abstractprogram into a list of values, the order of 
the elements in pieces is reflected in the output

processHelper: 
Recursion Diagram: 
OI: tle, [(Definition(Name("four"), NumE(4))), 
          (Expression(Name("four")))]
RI: tle w/ the definition added [(Expression(Name("four")))]
RO: Numv(4)
OO: NumV(4)
OI: tle, [(Definition(Name("four"), NumE(17))), 
          (Expression(Name("four")))]
RI: tle w/ the definition added [(Expression(Name("four")))]
RO: Numv(17)
OO: NumV(17)

Input: an environment and an abstractProgram, tle and pieces 
Output: conversion of the abstractProgram into a list of values, the order of 
the elements in pieces is reflected in the output. Every expression is evaluated 
in an environment in which every prior definition in that abstractProgram is added
to the tle. 


 */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/* 
Input: a rawProgram, program
Output:a a list of strings which is the result of rawProgram passed under readAll, 
parse, process, which results in a list of values, and each element in the list
of values is converted into a string under "stringOfValue" 
*/

let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));


/* CHECK EXPECTS */
/*Built In checkExpects */
checkExpect(plus([NumV(2), NumV(4)]), NumV(6), "plus: pos result 1")
checkExpect(plus([NumV(0), NumV(-6)]), NumV(-6), "plus: neg result")
checkExpect(plus([NumV(14), NumV(90)]), NumV(104), "plus: two big nums")
checkExpect(plus([NumV(-3), NumV(3)]), NumV(0), "plus: 0 result")
checkExpect(minus([NumV(2), NumV(4)]), NumV(-2), "minus: neg result")
checkExpect(minus([NumV(0), NumV(0)]), NumV(0), "minus: 0 result")
checkExpect(minus([NumV(18), NumV(12)]), NumV(6), "minus: pos result")
checkExpect(multiply([NumV(2), NumV(4)]), NumV(8), "multiply: pos result")
checkExpect(multiply([NumV(0), NumV(0)]), NumV(0), "mutiply: 0 result")
checkExpect(multiply([NumV(-1), NumV(4)]), NumV(-4), "multiply: neg result")
checkExpect(multiply([NumV(-1), NumV(-4)]), NumV(4), "multiply: 2 neg nums")
checkExpect(divide([NumV(-4), NumV(1)]), NumV(-4), "divide: neg result")
checkExpect(divide([NumV(-8), NumV(3)]), NumV(-2), "divide: rounding needed")
checkExpect(divide([NumV(8), NumV(9)]), NumV(0), "divide: rounding, 0 result")
checkExpect(divide([NumV(8), NumV(2)]), NumV(4), "divide: pos result")
checkExpect(divide([NumV(8), NumV(6)]), NumV(1), "divide: pos, rounding")
checkError(() => divide([NumV(1), NumV(0)]), "invalid division inputs, needs 2 elements of NumV() and 
  cannot divide by zero");  
checkExpect(remainder([NumV(4), NumV(3)]), NumV(1), "remainder: pos")
checkExpect(remainder([NumV(-8), NumV(3)]), NumV(-2), "remainder: neg")
checkExpect(remainder([NumV(8), NumV(-3)]), NumV(2), "remainder: neg, opp")
checkExpect(equalsNumP([NumV(4), NumV(3)]), BoolV(false), "equalsNumP: false, pos");
checkExpect(equalsNumP([NumV(0), NumV(0)]), BoolV(true), "equalsNumP: true, 0");
checkExpect(equalsNumP([NumV(-1), NumV(-4)]), BoolV(false), "equalsNumP: false, neg");
checkExpect(equalsNumP([NumV(-100), NumV(-100)]), BoolV(true), "equalsNumP: true, neg");
checkExpect(lessThanP([NumV(4), NumV(3)]), BoolV(false), "lessThanP: false, pos");
checkExpect(lessThanP([NumV(0), NumV(0)]), BoolV(false), "lessThanP:: false, 0");
checkExpect(lessThanP([NumV(-1), NumV(-4)]), BoolV(false), "lessThanP:: false, neg");
checkExpect(lessThanP([NumV(3), NumV(9)]), BoolV(true), "lessThanP:: true, pos");
checkExpect(lessThanP([NumV(-10), NumV(-9)]), BoolV(true), "lessThanP:: true, neg");
checkExpect(greaterThanP([NumV(4), NumV(3)]), BoolV(true), "greaterThanP: true, pos");
checkExpect(greaterThanP([NumV(0), NumV(0)]), BoolV(false), "greaterThanP:: false, 0");
checkExpect(greaterThanP([NumV(-1), NumV(-4)]), BoolV(true), "greaterThanP:: true, neg");
checkExpect(greaterThanP([NumV(3), NumV(9)]), BoolV(false), "greaterThanP:: false, pos");
checkExpect(greaterThanP([NumV(-10), NumV(-9)]), BoolV(false), "greaterThanP:: false, neg");
checkExpect(lessOrEqualP([NumV(4), NumV(3)]), BoolV(false), 
"lessOrEqualP: true, pos");
checkExpect(lessOrEqualP([NumV(0), NumV(0)]), BoolV(true), "lessOrEqualP: true 0");
checkExpect(lessOrEqualP([NumV(-1), NumV(-4)]), BoolV(false), 
"lessOrEqualP:: false, neg");
checkExpect(lessOrEqualP([NumV(3), NumV(9)]), BoolV(true), 
"lessOrEqualP:: true, pos");
checkExpect(lessOrEqualP([NumV(-10), NumV(-9)]), BoolV(true), 
"lessOrEqualP:: true, neg");
checkExpect(greaterOrEqualP([NumV(4), NumV(3)]), BoolV(true), 
"greaterOrEqualP: true, pos");
checkExpect(greaterOrEqualP([NumV(0), NumV(0)]), BoolV(true), 
"greaterOrEqualP: true 0");
checkExpect(greaterOrEqualP([NumV(-1), NumV(-4)]), BoolV(true), 
"greaterOrEqualP:: true, neg");
checkExpect(greaterOrEqualP([NumV(3), NumV(9)]), BoolV(false),
"greaterOrEqualP:: false, pos");
checkExpect(greaterOrEqualP([NumV(-10), NumV(-9)]), BoolV(false), 
"greaterOrEqualP:: false, neg");
checkExpect(equalP([NumV(4), NumV(4)]), BoolV(true), "equalP: true, NumV");
checkExpect(equalP([NumV(4), NumV(2)]), BoolV(false), "equalP: false, NumV");
checkExpect(equalP([ListV([NumV(4)]), ListV([NumV(4)])]), BoolV(true), 
"equalP:: true, ListV");
checkExpect(equalP([ListV([NumV(2)]), ListV([NumV(4)])]), BoolV(false), 
"equalP:: false, ListV");
checkExpect(equalP([BoolV(true), BoolV(true)]), BoolV(true), 
"equalP: true, BoolV");
checkExpect(equalP([BoolV(false), BoolV(true)]), BoolV(false), 
"equalP: false, BoolV");
checkExpect(numberP([NumV(4)]), BoolV(true), "numberP: true");
checkExpect(numberP([NumV(0)]), BoolV(true), "numberP: true, 0");
checkExpect(numberP([BoolV(true)]), BoolV(false), "numberP: false, BoolV");
checkExpect(numberP([ListV([NumV(3)])]), BoolV(false), "numberP: false, ListV");
checkExpect(zeroP([NumV(4)]), BoolV(false), "zeroP: false");
checkExpect(zeroP([NumV(0)]), BoolV(true), "zeroP: true");
checkExpect(consList([NumV(2), ListV([NumV(5)])]), ListV([NumV(2), NumV(5)]), 
"cons, into 1 element list");
checkExpect(consList([NumV(2), ListV([NumV(5), NumV(7)])]),
ListV([NumV(2), NumV(5), NumV(7)]), "cons, into 2 element list");
checkExpect(consList([NumV(2), ListV([])]), ListV([NumV(2)]), "cons, into empty list");
checkExpect(first([ListV([NumV(2), NumV(5)])]), NumV(2), "first, 2 element list");
checkExpect(first([ListV([BoolV(false)])]), BoolV(false), "first, 1 element list");
checkExpect(rest([ListV([NumV(2), NumV(5), NumV(2)])]), ListV([NumV(5), NumV(2)]), 
"rest, 3 element list");
checkExpect(rest([ListV([BoolV(false)])]), ListV([]), "rest, 1 element list");
checkExpect(emptyP([ListV([NumV(2), NumV(4)])]), BoolV(false),
 "emptyP, false, 3 element list");
checkExpect(emptyP([ListV([])]), BoolV(true), "emptyP, true, empty list");
checkExpect(emptyP([ListV([NumV(5)])]), BoolV(false), "emptyP, false, cons list");
checkExpect(consP([ListV([NumV(2), NumV(1)])]), BoolV(true), "consP, true, 2 element list");
checkExpect(consP([ListV([])]), BoolV(false), "consP, false, empty list");
checkExpect(notBI([BoolV(true)]), BoolV(false), "not, false");
checkExpect(notBI([BoolV(false)]), BoolV(true), "not, true");
/*Built In checkErrors*/
checkError(() => minus([NumV(1)]), "invalid subtraction inputs, needs 2"); 
checkError(() => plus([NumV(1)]), "invalid addition inputs, needs 2"); 
checkError(() => remainder([NumV(1)]), 
"invalid remainder inputs, needs 2 elements of NumV(), cannot
  divide by zero"); 
checkError(() => multiply([NumV(1)]), "invalid multiplication inputs, needs 2");
checkError(() => equalsNumP([NumV(1)]), "invalid equalNumP inputs, needs 2"); 
checkError(() => lessThanP([NumV(1)]), "invalid < inputs, needs 2"); 
checkError(() => greaterThanP([NumV(1)]), "invalid > inputs, needs 2"); 
checkError(() => lessOrEqualP([NumV(1)]), "invalid <= inputs, needs 2"); 
checkError(() => greaterOrEqualP([NumV(1)]), "invalid => inputs, needs 2"); 
checkError(() => equalP([NumV(1)]), "invalid equal? inputs, needs 2"); 
checkError(() => zeroP([BoolV(true)]), "invalid zeroP input, needs a NumV"); 
checkError(() => consList([BoolV(true)]), "invalid cons inputs, needs a NumV and a ListV"); 
checkError(() => first([NumV(1)]), "invalid first input, must be a nonempty ListV"); 
checkError(() => rest([NumV(1)]), "invalid rest input, must be a ListV"); 
checkError(() => emptyP([NumV(1)]), "invalid emptyP input, must be a ListV"); 
checkError(() => consP([NumV(1)]), "invalid consP input, must be a ListV"); 
checkError(() => notBI([NumV(1)]), "invalid not input, must be a BoolV"); 

/*parseExpression checkExpects */
checkExpectExpression(parseExpression(SymbolC("empty")), EmptyE,
  "parse: empty");
checkExpectExpression(parseExpression((read("(and true false)"))), 
AndE(BoolE(true), BoolE(false)), "parse: (and true false)");
checkExpectExpression(parseExpression((read("(or true false)"))),
 OrE(BoolE(true), BoolE(false)), "parse: (or true false)");
checkExpectExpression(parseExpression((read("(if true truecase falsecase)"))),
IfE(BoolE(true),NameE(Name("truecase")), NameE(Name("falsecase"))),
"parse: (if true (truecase) (falsecase))"); 
checkExpectExpression(parseExpression((read("(lambda (x) true)"))),
LambdaE([Name("x")], BoolE(true)), "parse: lambda single arg");
checkExpectExpression(parseExpression((read("(lambda (x y) true)"))), 
LambdaE([Name("x"), Name("y")], BoolE(true)), "parse: lambda two arg");
checkExpectExpression(parseExpression((read("(lambda (x y) (+ x y))"))), 
LambdaE([Name("x"), Name("y")], 
  ApplicationE([NameE(Name("+")), NameE(Name("x")), NameE(Name("y"))])),
"parse: lambda two arg with app expr"); 
checkExpectExpression(parseExpression((read("(cond ((empty? lst) true))"))),
CondE([(ApplicationE([NameE(Name("empty?")), NameE(Name("lst"))]), BoolE(true))]),
"cond with app expr");
checkExpectExpression(parseExpression((read("super"))), NameE(Name("super")),
"just a name"); 
checkExpectExpression(parseExpression((read("1"))), NumE(1), "just a num"); 
checkExpectExpression(parseExpression((read("true"))), BoolE(true), "just a bool"); 
checkExpectExpression(parseExpression((read("empty"))), EmptyE, "just an empty list"); 
checkExpectExpression(parseExpression((read("(let ((x 10)) x)"))), 
ApplicationE([NameE(Name("let")), 
  ApplicationE([ApplicationE([NameE(Name("x")), NumE(10)])]), NameE(Name("x"))]), 
  "one arg letexpr"); 
checkExpectExpression(parseExpression((read("(let ((x 10) (y 10)) (+ x y))"))), 
LetE([(Name("x"), NumE(10)), (Name("y"), NumE(10))],
   ApplicationE([NameE(Name("+")), NameE(Name("x")), NameE(Name("y"))])), 
   "two arg letexpr with procs"); 

/* eval checkExpects */
checkExpect(rackette
("(define n 5) n"), ["5"], "2 piece, define (1 arg)/just lookup");
checkExpect(rackette
("(define n 5) (+ n 3)"), ["8"], "2 piece, define (1 arg)/builtin");
checkExpect(rackette("+"), ["<builtin-plus>"], "1 piece, builtin, ");


checkExpect(rackette
("(let ((z 2)) (let ((x 5) (y 2)) (+ x z)))"), ["7"], 
            "let (2 arg) in let (1 arg), proc app body w builtin");

checkExpect(eval(initialTle, [], parseExpression(read("(lambda (x) true)"))),
            ClosureV([Name("x")], BoolE(true), []),
            "eval/parseExpression: lambda (unapplied) => closure");

checkExpect(eval(initialTle, [], parseExpression(read("5"))), 
            NumV(5),
            "eval/parseExpression: num => NumV");

checkExpect(eval(initialTle, [], parseExpression(read("true"))), 
            BoolV(true),
            "eval/parseExpression: bool => BoolV(true)");

checkExpect(eval(initialTle, [], parseExpression(read("false"))), 
            BoolV(false),
            "eval/parseExpression: bool => BoolV(false)");

checkExpect(eval(initialTle, [], parseExpression(read("empty"))), 
            ListV([]),
            "eval/parseExpression: bool => BoolV");

checkExpect(eval(initialTle, [], parseExpression(read("(and (> 3 5) (< 1 5))"))),
            BoolV(false),
            "eval/parseExpression: bool => BoolV");
checkExpect(process(parse(readAll("(define ultimatecheck (lambda (x y) (cond
                                      ((and (empty? x) (empty? y)) true)
                                      ((or (cons? x) (cons? y)) false))))
                                  (ultimatecheck (cons 1 empty) (cons 2 empty))"))), 
[BoolV(false)], 
"process/parse/readAll: includes
cond, lambda, user defined proc, 
lists (uses parseDefinition, parseExpression, eval)");

checkExpect(process(parse(readAll("((lambda (x y) (+ x y)) 2 4)"))), 
            [NumV(6)], 
            "process/parse/readAll: lambda")

checkExpect(process(parse(readAll("(let ((x 2) (y 4)) (+ x y))"))), 
            [NumV(6)], 
            "process/parse/readAll: let")
 
checkExpect(process(parse(readAll("(+ 2 3)"))), 
            [NumV(5)], 
            "process/parse/readAll: builtin ExpressionE +")

checkExpect(process(parse(readAll("(zero? 4)"))),
           [BoolV(false)], 
           "process/parse/readAll: builtin ExpressionE")

/*rackette checkExpects*/
checkExpect(rackette("(+ 1 2)"), ["3"], "builtin"); 
checkExpect(rackette("(define charlotte 19) charlotte"),
   ["19"], 
   "define"); 
checkExpect(rackette("(not true)"), ["false"], "not"); 
checkExpect(rackette("((lambda (x y) (+ x y)) 1 2)"), ["3"], "lambda proc app"); 
checkExpect(rackette("(define ultimatecheck (lambda (x y) (cond
                                      ((and (empty? x) (empty? y)) true)
                                      ((or (cons? x) (cons? y)) false))))
(ultimatecheck (cons 1 empty) (cons 2 empty))"), ["false"], "big check: includes
cond, lambda, user defined proc, lists"); 



