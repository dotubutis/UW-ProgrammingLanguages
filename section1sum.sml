
(* Function bindings *)
fun pow (x:int, y:int) = (* correct only for y >= 0 *)
    if y=0
    then 1
    else x * pow(x,y-1)

fun cube (x:int) =
    pow(x,3)

val ans = cube(4)

(* Pairs and Tuples *)

fun swap (pr : int*bool) =
    (#2 pr, #1 pr)

fun sum_two_pairs (pr1 : int*int, pr2 : int*int) =
    (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod (x : int, y : int) = (* note: returning a pair is a real pain in Java *)
    (x div y, x mod y)

fun sort_pair (pr : int*int) =
    if (#1 pr) < (#2 pr)
    then pr
    else ((#2 pr),(#1 pr))

(* Lists *)

fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd(xs) + sum_list(tl xs)

fun countdown (x : int) =
    if x=0
    then []
    else x :: countdown(x-1)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else (#1 (hd xs))::(firsts(tl xs))

fun seconds (xs : (int * int) list) =
    if null xs
    then []
    else (#2 (hd xs))::(seconds(tl xs))

fun sum_pair_list2 (xs : (int * int) list) =
    (sum_list (firsts xs)) + (sum_list (seconds xs))

(* Let expressions *)

let val x = 1
in
    (let val x = 2 in x+1 end) + (let val y = x+2 in y+1 end)
end

fun countup_from1 (x:int) =
    let fun count (from:int, to:int) =
        if from=to
        then to::[]
        else from :: count(from+1,to)
    in
        count(1,x)
    end

fun countup_from1_better (x:int) =
    let fun count (from:int) =
            if from=x
            then x::[]
            else from :: count(from+1)
    in
        count 1
    end

fun bad_max (xs : int list) =
    if null xs
    then 0 (* note: bad style; see below *)
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)

fun good_max (xs : int list) =
    if null xs
    then 0 (* note: bad style; see below *)
    else if null (tl xs)
    then hd xs
    else
        (* for style, could also use a let-binding for hd xs *)
        let val tl_ans = good_max(tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end

(* Options *)

fun better_max (xs : int list) =
    if null xs
    then NONE
    else
        let val tl_ans = better_max(tl xs)
        in if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME (hd xs)
        end

fun better_max2 (xs : int list) =
    if null xs
    then NONE
    else let (* fine to assume argument nonempty because it is local *)
            fun max_nonempty (xs : int list) =
                if null (tl xs) (* xs must not be [] *)
                then hd xs
                else let val tl_ans = max_nonempty(tl xs)
                     in
                        if hd xs > tl_ans
                        then hd xs
                        else tl_ans
                     end
         in
            SOME (max_nonempty xs)
         end

(*  Some Other Expressions and Operators *)

(* andalso performs logical 'and' *)
val e1 = true
val e2 = false
val result = e1 andalso e2  (* result: false *)
val alternativeResult = if e1 then e2 else false  (* alternativeResult: false *)

(* orelse performs logical 'or' *)
val e1 = true
val e2 = false
val result = e1 orelse e2  (* result: true *)
val alternativeResult = if e1 then true else e2  (* alternativeResult: true *)

(* 'not' performs logical negation *)
val e = true
val result = not e  (* result: false *)
(* equivalent to: *)
val alternativeResult = if e then false else true  (* alternativeResult: false *)

(* '=' compares values for equality *)
val e1 = 5
val e2 = 10
val result = (e1 = e2)  (* result: false *)

(* '<>' compares values for inequality *)
val e1 = 5
val e2 = 10
val result = (e1 <> e2)  (* result: true *)

(* Various arithmetic comparisons *)
val x = 5
val y = 10

val result1 = (x > y)  (* result1: false *)
val result2 = (x < y)  (* result2: true *)
val result3 = (x >= y) (* result3: false *)
val result4 = (x <= y) (* result4: true *)

(* '-' is used for subtraction, '~' for negation *)
val e1 = 10
val e2 = 7

val result = e1 - e2  (* result: 3 *)
val negated = ~e2     (* negated: -7 *)
