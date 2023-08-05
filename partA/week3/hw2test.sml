(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework2.sml";

val test1a = all_except_option ("string", ["string"]) = SOME []
val test1b = all_except_option("string", ["string", "other"]) = SOME ["other"]
val test1c = all_except_option("string", ["other", "string"]) = SOME ["other"]
val test1d = all_except_option("string", []) = NONE
val test1e = all_except_option("string", ["other1", "other2", "other3"]) = NONE
val test1f = all_except_option("string", ["string", "other2", "other3"]) = SOME ["other2", "other3"]
val test1g = all_except_option("Fred", ["Freddie","Fred","F"]) = SOME ["Freddie","F"]

val test2a = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2c = get_substitutions1 ([["Fred","Freddie","Fredrick"],["Betty","Elizabeth"]], "Fred") = ["Freddie", "Fredrick"]
val test2d = get_substitutions1 ([[]], "Fred") = []
val test2e = get_substitutions1 ([["Alice","Bob"],["Carol","Dave"]], "Eve") = []
val test2f = get_substitutions1 ([["John", "Johnny", "Jack"],["Jack","Jill"],["Johnny", "Jonathan"]], "Johnny") = ["John", "Jack", "Jonathan"]
val test2g = get_substitutions1 ([["Fred"],["Betty","Elizabeth"],["Johnny","John"]], "Fred") = []
val test2h = get_substitutions1 ([["Fred"],["Fred"],["Johnny","John"]], "Fred") = []

val test3x = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3a = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3b = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3c = get_substitutions2 ([["Fred","Freddie","Fredrick"],["Betty","Elizabeth"]], "Fred") = ["Freddie", "Fredrick"]
val test3d = get_substitutions2 ([[]], "Fred") = []
val test3e = get_substitutions2 ([["Alice","Bob"],["Carol","Dave"]], "Eve") = []
val test3f = get_substitutions2 ([["John", "Johnny", "Jack"],["Jack","Jill"],["Johnny", "Jonathan"]], "Johnny") = ["John", "Jack", "Jonathan"]

val test3g = get_substitutions2 ([["Fred"],["Betty","Elizabeth"],["Johnny","John"]], "Fred") = []
val test3h = get_substitutions2 ([["Fred"],["Fred"],["Johnny","John"]], "Fred") = []


val test4a = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test4b = similar_names ([["John","Jon"],["Elizabeth","Betty"],["Fred","Freddie","F"]], {first="John", middle="Doe", last="Smith"}) =
    [{first="John", last="Smith", middle="Doe"}, {first="Jon", last="Smith", middle="Doe"}]
val test4c = similar_names ([["James","Jim","Jimmy"],["Michael","Mike"],["Thomas","Tom","Tommy"]], {first="James", middle="M", last="Johnson"}) =
    [{first="James", last="Johnson", middle="M"}, {first="Jim", last="Johnson", middle="M"}, {first="Jimmy", last="Johnson", middle="M"}]
val test4d = similar_names ([["Katherine","Kathy","Kate"],["Jennifer","Jen","Jenny"],["Rebecca","Becky","Becca"]], {first="Katherine", middle="R", last="Thompson"}) =
    [{first="Katherine", last="Thompson", middle="R"}, {first="Kathy", last="Thompson", middle="R"}, {first="Kate", last="Thompson", middle="R"}]
val test4e = similar_names ([["Patricia","Pat","Patty"],["Robert","Bob","Robbie"],["Charles","Charlie","Chuck"]], {first="Patricia", middle="E", last="Wilson"}) =
    [{first="Patricia", last="Wilson", middle="E"}, {first="Pat", last="Wilson", middle="E"}, {first="Patty", last="Wilson", middle="E"}]
val test4f = similar_names ([["Daniel","Dan","Danny"],["Richard","Rich","Rick"],["William","Will","Billy"]], {first="Daniel", middle="A", last="Brown"}) =
    [{first="Daniel", last="Brown", middle="A"}, {first="Dan", last="Brown", middle="A"}, {first="Danny", last="Brown", middle="A"}]


(* val test4a_result = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) *)

(* val test5 = card_color (Clubs, Num 2) = Black *)

(* val test6 = card_value (Clubs, Num 2) = 2 *)

(* val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [] *)

(* val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true *)

(* val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4 *)

(* val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4 *)

(* val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6 *)

(* val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3 *)

(* val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) *)
             
             
