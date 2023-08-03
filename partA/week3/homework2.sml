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
