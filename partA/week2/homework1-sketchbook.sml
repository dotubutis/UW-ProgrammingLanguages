(* You will write 11 SML functions (and tests for them) related to calendar dates. In all problems, a “date”
is an SML value of type int*int*int, where the first part is the year, the second part is the month, and
the third part is the day. A “reasonable” date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable
dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will
naturally work correctly for some/all non-reasonable dates. A “day of year” is a number from 1 to 365
where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.) *)

(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_older (x: int*int*int, y: int*int*int) =
    if (#1 x) < (#1 y) then true
    else 
        if (#1 x) = (#1 y) then 
            if (#2 x) < (#2 y) then true
            else 
                if (#2 x) = (#2 y) then 
                    if (#3 x) < (#3 y) then true
                    else false
                else false
        else false

(* Uses pattern matching to unpack arguments *)
fun is_older ((y1, m1, d1): int*int*int, (y2, m2, d2): int*int*int) = 
    (y1 < y2) orelse (y1 = y2 andalso (m1 < m2 orelse (m1 = m2 andalso d1 < d2)))

(* Concise without pattern matching to unpack arguments *)
fun is_older (x: int*int*int, y: int*int*int) =
    (#1 x < #1 y) orelse (#1 x = #1 y andalso (#2 x < #2 y orelse (#2 x = #2 y andalso #3 x < #3 y)))


(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given *)
fun dates_in_month (dates: (int*int*int) list, month: int) = 
    if null dates then []
    else
        if #2 (hd dates) = month
        then (hd dates)::dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* Refactored *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates then []
    else
        let
            val date = hd dates
            val remainingDates = tl dates
        in
            if #2 date = month
            then date::dates_in_month(remainingDates, month)
            else dates_in_month(remainingDates, month)
        end

(* Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

(* Inefficient approach *)
fun number_before_reaching_sum(sum: int, nums: int list) = 
    let 
        fun sum_n (xs: int list, n: int) =
            if n = 0 then 0
            else hd(xs) + sum_n(tl xs, n - 1)

        fun find_answer(n: int) =
            if sum_n(nums, n) >= sum 
            then n - 1
            else find_answer(n + 1)
    in 
        find_answer(1)
    end

(* Efficient solution *)
fun number_before_reaching_sum(sum: int, nums: int list) =
    let
        fun helper (accSum: int, count: int, xs: int list) =
            if accSum >= sum then count - 1
            else helper(accSum + hd(xs), count + 1, tl xs)
    in
        helper(0, 0, nums)
    end

