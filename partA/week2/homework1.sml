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


