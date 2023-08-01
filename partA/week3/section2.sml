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
(* 1) `Constant i` - This case matches when `e` is a `Constant`. As the constant represents a single value and has no addition operations, the function returns `0`.
2) `Negate e2` - This case matches when `e` is a `Negate` node. Since `Negate` also represents a single unary operation (negation), there are no addition operations involved. The function then recursively explores `e2`, which is the operand for the negate operation, to see if there are any additions within that.
3) `Add(e1, e2)` - This case matches when `e` is an `Add`. The function returns `1` (for this addition operation) plus the total number of additions in `e1` and `e2`, the operands for the addition operation.
4) `Multiply(e1, e2)` - This case matches when `e` is a `Multiply`. Since multiplication doesn't count as an addition according to this function, the function recursively explores `e1` and `e2`, which are the operands for the multiplication operation, to count any addition operations within them. *)

(* *** *)
(* Lists and Options are Datatypes *)
(* *** *)

(* Because datatype definitions can be recursive, we can use them to create our own types for lists. For example,
this binding works well for a linked list of integers: *)
datatype my_int_list = Empty
                     | Cons of int * my_int_list

val one_two_three = Cons(1,Cons(2,Cons(3,Empty)))

fun append_mylist (xs,ys) =
  case xs of
      Empty => ys
    | Cons(x,xs’) => Cons(x, append_mylist(xs’,ys))

(* For options, all you need to know is SOME and NONE are constructors, which we use to create values (just like
before) and in patterns to access the values. Here is a short example of the latter *)
fun inc_or_zero intoption =
  case intoption of
      NONE => 0
    | SOME i => i+1

(* The story for lists is similar with a few convenient syntactic peculiarities: [] really is a constructor that
carries nothing and :: really is a constructor that carries two things, but :: is unusual because it is an infix
operator (it is placed between its two operands), both when creating things and in patterns *)
fun sum_list xs =
  case xs of
      [] => 0
    | x::xs’ => x + sum_list xs’

fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs’ => x :: append(xs’,ys)

(* Notice here x and xs’ are nothing but local variables introduced via pattern-matching. We can use any
names for the variables we want. We could even use hd and tl — doing so would simply shadow the functions
predefined in the outer environment.

The reasons why you should usually prefer pattern-matching for accessing lists and options instead of func-
tions like null and hd is the same as for datatype bindings in general: you cannot forget cases, you cannot
apply the wrong function, etc. *)

(* *** *)
(* Polymorphic Datatypes *)
(* *** *)

(* this is exactly how options are pre-defined in the
environmen *)
datatype ’a option = NONE | SOME of ’a

(* binary tree where internal nodes hold values of type ’a and leaves hold values of type ’b *)
datatype (’a,’b) tree = Node of ’a * (’a,’b) tree * (’a,’b) tree
| Leaf of ’b

(* *** *)
(* Pattern-Matching for Each-Of Types: The Truth About Val-Bindings *)
(* *** *)

(* function for summing the three parts of an
int * int * int *)
fun sum_triple (triple : int * int * int) =
    case triple of
      (x,y,z) => z + y + x

(* And a similar example with records (and ML’s string-concatenation operator) could look like this: *)
fun full_name (r : {first:string,middle:string,last:string}) =
    case r of
        {first=x,middle=y,last=z} => x ^ " " ^ y ^ " " ^z
    
(* However, a case-expression with one branch is poor style — it looks strange because the purpose of such
expressions is to distinguish cases, plural.  *)
(* this approach is better style *)

fun full_name (r : {first:string,middle:string,last:string}) =
    let val {first=x,middle=y,last=z} = r
    in
        x ^ " " ^ y ^ " " ^z
    end

fun sum_triple (triple : int*int*int) =
    let val (x,y,z) = triple
    in
        x + y + z
    end

(* even better: *)
(* use a pattern when defining a function binding and the pattern will be used to introduce bindings by matching against the value passed when the
function is called *)
fun full_name {first=x,middle=y,last=z} =
    x ^ " " ^ y ^ " " ^z

fun sum_triple (x,y,z) =
    x + y + z


(* *** *)
(* Type Inference *)
(* *** *)

(* This version needs an explicit type on the argument: *)
fun sum_triple (triple : int * int * int) =
    #1 triple + #2 triple + #3 triple
(* type-checker cannot  infer that the argument must have type int*int*int, since it could also have type int*int*int*int
or int*int*int*string or int*int*int*bool*string or an infinite number of other types. If you do not
use #, ML almost never requires explicit type annotations thanks to the convenience of type inference. *)

(* *** *)
(* Digression: Polymorphic Types and Equality Types *)
(* *** *)
fun same_thing(x,y) = if x=y then "yes" else "no" (* has type ’’a * ’’a -> string *)
fun is_three x = if x=3 then "yes" else "no" (* has type int -> string *)


(* *** *)
(* Nested Patterns *)
(* *** *)

fun len xs =
    case xs of
      [] => 0
    | x::xs’ => 1 + len xs’

(* better style to use a wildcard pattern *)
(* A wildcard pattern (_) matches any value v and introduces no bindings. *)
fun len xs =
    case xs of
      [] => 0
    | _::xs’ => 1 + len xs’

exception BadTriple

fun zip3 list_triple =
    case list_triple of
        ([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise BadTriple

fun unzip3 lst =
    case lst of
        [] => ([],[],[])
      | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                       in
                          (a::l1,b::l2,c::l3)
                       end

(* check that a list of integers is sorted *)
fun nondecreasing intlist =
    case intlist of
        [] => true
      | _::[] => true
      | head::(neck::rest) => (head <= neck andalso nondecreasing (neck::rest))

(* It is also sometimes elegant to compare two values by matching against a pair of them. This example, for
determining the sign that a multiplication would have without performing the multiplication, is a bit silly
but demonstrates the idea: *)
datatype sgn = P | N | Z

fun multsign (x1,x2) =
  let fun sign x = if x=0 then Z else if x>0 then P else N
  in
      case (sign x1,sign x2) of
          (Z,_) => Z
        | (_,Z) => Z
        | (P,P) => P
        | (N,N) => P
        | _ => N (* many say bad style; I am okay with it *)
  end

(* The style of this last case deserves discussion: When you include a “catch-all” case at the bottom like this,
you are giving up any checking that you did not forget any cases: after all, it matches anything the earlier
cases did not, so the type-checker will certainly not think you forgot any cases. So you need to be extra
careful if using this sort of technique and it is probably less error-prone to enumerate the remaining cases
(in this case (N,P) and (P,N)). That the type-checker will then still determine that no cases are missing is
useful and non-trivial since it has to reason about the use (Z,_) and (_,Z) to figure out that there are no
missing possibilities of type sgn * sgn *)


(* *** *)
(* Optional: Multiple Cases in a Function Binding *)
(* *** *)

(* match against one-of types in val/function bindings *)

datatype exp = Constant of int | Negate of exp | Add of exp * exp | Multiply of exp * exp

fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1,e2)) = (eval e1) * (eval e2)

fun append ([],ys) = ys
  | append (x::xs’,ys) = x :: append(xs’,ys)

(* As a matter of semantics, it is just syntactic sugar for a single function body that is a case expression *)
fun eval e =
    case e of
        Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) * (eval e2)

fun append e =
    case e of
      ([],ys) => ys
    | (x::xs’,ys) => x :: append(xs’,ys)

(* *** *)
(* Exceptions *)
(* *** *)

(* You can create your own kinds of exceptions with an exception binding. Exceptions can optionally carry
values with them, which let the code raising the exception provide more information *)

exception MyUndesirableCondition
exception MyOtherException of int * int

 (* here is a version of a function
that returns the maximum element in a list of integers. Rather than return an option or raise a particular
exception like List.Empty if called with [], it takes an argument of type exn and raises it. *)
fun maxlist (xs,ex) =
    case xs of
        [] => raise ex
      | x::[] => x
      | x::xs’ => Int.max(x,maxlist(xs’,ex))

(* *** *)
(* Tail Recursion and Accumulators *)
(* *** *)

(* No tail recursion *)
fun sum1 xs =
    case xs of
        [] => 0
      | i::xs’ => i + sum1 xs’

(* Tail recursion *)
fun sum2 xs =
    let fun f (xs,acc) =
            case xs of
                [] => acc
              | i::xs’ => f(xs’,i+acc)
    in
        f(xs,0)
    end

(* No tail recursion *)
fun fact1 n = if n=0 then 1 else n * fact1(n-1)

(* Tail recursion *)
fun fact2 n =
    let fun aux(n,acc) = if n=0 then acc else aux(n-1,acc*n)
    in
        aux(n,1)
    end

(* No tail recursion *)
fun rev1 lst =
    case lst of
      [] => []
    | x::xs => (rev1 xs) @ [x]

(* Tail recursion *)
fun rev2 lst =
    let fun aux(lst,acc) =
            case lst of
                [] => acc
              | x::xs => aux(xs, x::acc)
    in
        aux(lst,[])
    end