(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(str, strlist) =
    let
        fun f (xs, acc, found) =
            case xs of
                [] => if found then SOME (rev acc) else NONE
              | x::xs' => if same_string(str, x) then f(xs', acc, true) else f(xs', x::acc, found)
    in
        f(strlist, [], false)
    end
(* 
Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions ) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. Example:
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
(* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
both in more than one list in substitutions. Example:
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)

fun get_substitutions1(substitutions, str) = 
    case substitutions of
        [] => []
        | x::xs' => case all_except_option(str, x) of 
                        NONE => get_substitutions1(xs', str)
                        | SOME(ys) => ys@get_substitutions1(xs', str)

(* Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2(substitutions, str) = 
    let
        fun f (xs, acc) =
            case xs of
                [] => acc
                | x::xs' => case all_except_option(str, x) of 
                                NONE => f(xs', acc)
                                | SOME(ys) => f(xs', acc@ys)
    in
        f(substitutions, [])
    end

(* Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). Example:

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})

(* answer: [{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}] *)

Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
around 10 lines. *)

fun similar_names(substitutions, {first, middle, last}) =
    let 
        fun f (xs, acc) = 
            case xs of 
                [] => List.rev acc
                | x::xs' => f(xs', {first=x, middle=middle, last=last}::acc)
    in 
        f(get_substitutions2(substitutions, first), [{first=first, middle=middle, last=last}])
    end