(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework1.sml";

val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2001,2,3),(2002,3,4)) = true  (* earlier year *)
val test1e = is_older ((2023,2,3),(2022,5,4)) = false  (* later year *)
val test1g = is_older ((2022,7,15),(2022,7,16)) = true  (* same year, same month, earlier day *)
val test1h = is_older ((2022,7,16),(2022,7,15)) = false  (* same year, same month, later day *)

val test2a = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month ([(2012,5,28),(2013,5,30),(2015,7,14),(2022,5,20)],5) = 3
val test2g = number_in_month ([(2023,2,28)],2) = 1
val test2h = number_in_month ([],2) = 0
val test2k = number_in_month ([(2022,12,12),(2023,12,12),(2023,1,1)],12) = 2

val test3a = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months ([(2013,1,1),(2014,2,2),(2015,3,3),(2016,4,4)],[1,2,3,4]) = 4
val test3d = number_in_months ([(2024,9,9),(2025,10,10),(2026,11,11),(2027,12,12)],[9,10,11,12]) = 4
val test3j = number_in_months ([], [5,6,7,8]) = 0

val test4a = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month ([(2014,2,28),(2014,2,14),(2015,2,28)],2) = [(2014,2,28),(2014,2,14),(2015,2,28)]
val test4c = dates_in_month ([(2016,1,1),(2017,1,1),(2018,1,1)],1) = [(2016,1,1),(2017,1,1),(2018,1,1)]
val test4h = dates_in_month ([],1) = []
val test4k = dates_in_month ([(2025,12,31),(2026,1,1),(2026,12,31)],12) = [(2025,12,31),(2026,12,31)]

val test5a = dates_in_months ([(2014,2,14),(2014,3,15),(2014,4,16),(2014,5,17)],[2,3,4]) = [(2014,2,14),(2014,3,15),(2014,4,16)]
val test5b = dates_in_months ([(2016,10,22),(2016,11,23),(2016,12,24),(2016,1,25)],[10,11,12]) = [(2016,10,22),(2016,11,23),(2016,12,24)]
val test5c = dates_in_months ([(2017,2,26),(2017,3,27),(2017,4,28),(2017,5,29)],[2,3]) = [(2017,2,26),(2017,3,27)]
val test5d = dates_in_months ([(2020,2,7),(2020,3,8),(2020,4,9),(2020,5,10)],[]) = []
val test5e = dates_in_months ([], [2,3,4]) = []

val test6a = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6b = get_nth (["Hello"], 1) = "Hello" (* Test with a list of length 1 *)
val test6c = get_nth (["The", "quick", "brown", "fox"], 4) = "fox" (* Test with n being the last element in the list *)
val test6d = get_nth (["apple", "banana", "cherry", "date", "elderberry", "fig", "grape"], 7) = "grape" (* Test with n equals to the length of the list *)

val test7a = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7b = date_to_string (2023, 12, 31) = "December 31, 2023" (* Test with last day of the year *)
val test7c = date_to_string (1999, 1, 1) = "January 1, 1999" (* Test with first day of the year *)
val test7d = date_to_string (2000, 2, 29) = "February 29, 2000" (* Test with leap year day *)
val test7e = date_to_string (1800, 7, 4) = "July 4, 1800" (* Test with historical date *)

val test8a = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum (15, [5,5,5,5,5]) = 2 (* Test where multiple elements have the same value *)
val test8c = number_before_reaching_sum (1, [1,1,1,1,1,1,1]) = 0 (* Test where the sum is reached at the first element *)
val test8d = number_before_reaching_sum (100, [10,20,30,40,50,60]) = 3 (* Test with larger numbers *)
val test8e = number_before_reaching_sum (7, [3,3,1,1,1,1]) = 2 

val test9a = what_month 1 = 1 (* The first day of the year, January *)
val test9b = what_month 32 = 2 (* The day after January ends, February *)
val test9c = what_month 365 = 12 
val test9d = what_month 160 = 6 
val test9e = what_month 274 = 10 

val test10a = month_range (31, 34) = [1,2,2,2]
val test10b = month_range (1, 1) = [1]
val test10c = month_range (60, 70) = [3,3,3,3,3,3,3,3,3,3,3] 

(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
val test11a = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11b = oldest([(2000,1,1),(1999,3,31),(1999,4,28)]) = SOME (1999,3,31) 
val test11c = oldest([(2022,3,20),(2021,12,10),(2021,12,9)]) = SOME (2021,12,9) 
val test11d = oldest([(1998,7,14),(1983,6,18),(1988,3,12)]) = SOME (1983,6,18) 
val test11e = oldest([]) = NONE


val test12a = number_in_months_challenge ([(2012,2,28),(2013,12,1)], [2, 2]) = 1
val test12b = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2015,2,2)], [2,2,2]) = 2
val test12c = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2015,2,2),(2014,8,8)], [8,8,2,2]) = 3
val test12d = number_in_months_challenge ([(2012,2,28)],[]) = 0
val test12e = number_in_months_challenge ([], [2, 2]) = 0
val test12f = dates_in_months_challenge ([(2014,2,14),(2014,3,15),(2014,4,16),(2014,5,17)],[2,3,3,4]) = [(2014,2,14),(2014,3,15),(2014,4,16)]
val test12g = dates_in_months_challenge ([(2016,10,22),(2016,11,23),(2016,12,24),(2016,1,25)],[10,10,11,12]) = [(2016,10,22),(2016,11,23),(2016,12,24)]
val test12h = dates_in_months_challenge ([(2017,2,26),(2017,3,27),(2017,4,28),(2017,5,29)],[2,3,3]) = [(2017,2,26),(2017,3,27)]
