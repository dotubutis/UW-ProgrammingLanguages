(* Roughly, this defines a new type where values have an int * int or a string or nothing. Any value
will also be “tagged” with information that lets us know which variant it is: These “tags,” which we will
call constructors, are TwoInts, Str, and Pizza. *)
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x = (* f has type mytype -> int *)
    case x of
        Pizza => 3
      | TwoInts(i1,i2) => i1 + i2
      | Str s => String.size s

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
            | Name of string * (string option) * string

(* Unfortunately, this sort of example is one where programmers often show a profound lack of understanding
of one-of types and insist on using each-of types, which is like using a saw as a hammer (it works, but you
are doing the wrong thing). Consider BAD code like this: *)

(* If student_num is -1, then use the other fields, otherwise ignore other fields *)
{student_num : int, first : string, middle : string option, last : string}

(* On the other hand, each-of types are exactly the right approach if we want to store for each student their
id-number (if they have one) and their full name: *)
{ student_num : int option,
first : string,
middle : string option,
last : string }

(* Our last example is a data definition for arithmetic expressions containing constants, negations, additions,
and multiplications *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

(* Thanks to the self-reference, what this data definition really describes is trees where the leaves are integers
and the internal nodes are either negations with one child, additions with two children or multiplications
with two children. We can write a function that takes an exp and evaluates it: *)
fun eval e =
    case e of
          Constant i => i
        | Negate e2 => ~ (eval e2)
        | Add(e1,e2) => (eval e1) + (eval e2)
        | Multiply(e1,e2) => (eval e1) * (eval e2)

(* Evaluates to 15 *)
eval (Add (Constant 19, Negate (Constant 4)))

(* compute the number of addition operations in an abstract syntax tree (AST) *)
fun number_of_adds e =
    case e of
          Constant i => 0
        | Negate e2 => number_of_adds e2
        | Add(e1,e2) => 1 + number_of_adds e1 + number_of_adds e2
        | Multiply(e1,e2) => number_of_adds e1 + number_of_adds e2

(* GPT-4 explanation *)
1) `Constant i` - This case matches when `e` is a `Constant`. As the constant represents a single value and has no addition operations, the function returns `0`.
2) `Negate e2` - This case matches when `e` is a `Negate` node. Since `Negate` also represents a single unary operation (negation), there are no addition operations involved. The function then recursively explores `e2`, which is the operand for the negate operation, to see if there are any additions within that.
3) `Add(e1, e2)` - This case matches when `e` is an `Add`. The function returns `1` (for this addition operation) plus the total number of additions in `e1` and `e2`, the operands for the addition operation.
4) `Multiply(e1, e2)` - This case matches when `e` is a `Multiply`. Since multiplication doesn't count as an addition according to this function, the function recursively explores `e1` and `e2`, which are the operands for the multiplication operation, to count any addition operations within them.
