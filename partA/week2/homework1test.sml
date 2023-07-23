(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework1.sml";

val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2001,2,3),(2002,3,4)) = true  (* earlier year *)
val test1c = is_older ((2005,6,7),(2005,6,7)) = false  (* same date *)
val test1d = is_older ((2003,2,1),(2004,5,6)) = true  (* earlier year *)
val test1e = is_older ((2023,2,3),(2022,5,4)) = false  (* later year *)
val test1f = is_older ((2022,6,15),(2022,7,5)) = true  (* same year, earlier month *)
val test1g = is_older ((2022,7,15),(2022,7,16)) = true  (* same year, same month, earlier day *)
val test1h = is_older ((2022,7,16),(2022,7,15)) = false  (* same year, same month, later day *)

(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)