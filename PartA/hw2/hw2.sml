(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 
 * put your solutions for problem 1 here 
 *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if thestring is not in the list,
   else return SOME lst where lst is identical to the argument list except the stringis not in it. You may assume the string is 
   in the list at most once. Use same_string, provided to you,to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(str, xs) = 
    let 
        fun two_lists(list, previous_list) = 
          case list  of 
            [] => NONE
            | x::xs' => if same_string(str, x)
                        then SOME (previous_list@xs')
                        else two_lists(xs', previous_list@[x])
    in
      two_lists(xs, [])
    end

(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, thesubstitutions)
 and a string s and returns a string list. The result has all the strings that are insome list in substitutions that also has s, 
 but s itself should not be in the result. Example:get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
 (* answer: ["Fredrick","Freddie","F"] *)Assume each list in substitutions has no repeats.
 The result will have repeats if s and another string areboth in more than one list in substitutions.
 Example:get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
 (* answer: ["Jeffrey","Geoff","Jeffrey"] *)  *)
fun get_substitutions1 (xss, str) =
  case xss of
      [] => []
    | x::xs' => case all_except_option(str, x) of
                    NONE => get_substitutions1(xs', str)
                  | SOME y => y @ get_substitutions1(xs', str)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursivelocal helper function. 
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ~~ LOL! I'm already did it :D ~~ 
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
fun get_substitutions2(xss, str) = get_substitutions1(xss, str)

(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and(c)) and a full name 
of type {first:string,middle:string,last:string} and returns a list of fullnames (type {first:string,middle:string,last:string} list). 
The result is all the full names youcan produce by substituting for the first name (and only the first name) 
using substitutions and parts (b)or (c). The answer should begin with the original name (then have 0 or more other names). 
Example:similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) 
answer: [{first="Fred", last="Smith", middle="W"},
         {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"},
         {first="F", last="Smith", middle="W"}] *)
fun similar_names(xss, {first=f, middle=m, last=l}) = 
    let 
      val other_1s = f::get_substitutions1(xss, f)
    in
      let
      fun generate_answer(sub_firsts, names) =
        case sub_firsts of
          [] => names
        | x::xs' => generate_answer(xs', names@[{first = x, middle=m, last=l}])
      in
      generate_answer(other_1s, [])
    end
  end

(* =================================================================================== *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*
 * put your solutions for problem 2 here 
 *)

(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,diamonds and hearts 
are red). Note: One case-expression is enough. *)
fun card_color(card, _) = 
  case card of
    (Hearts | Diamonds) => Red
    | _ => Black

(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have theirnumber as the value, 
aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value(_, card) =
  case card of
      Num x => x
    | Ace => 11
    | _   => 10

(* (c) Write a function remove_card, which takes a list of cards cards, a card c, and an exception e. 
It returns alist that has all the elements of cards except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e.  *)
fun remove_card(cards, c, e) =
  case cards of
      [] => raise e
    | x::xs' => if same_card(c, x)
                then xs'
                else x :: remove_card(xs', c, e)_exp()
  end

(* (d) Write a function all_same_color, which takes a list of cards and returns true
   if all the cards in thelist are the same color.  *)
fun all_same_color(cards) = 
  case cards of
      [] => true
    | x::[] => true
    | f::s::xs' => if card_color(f) = card_color(s)
                  then all_same_color(s::xs')
                  else false
                
(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values.  *)
fun sum_cards(cards) = 
  let 
    fun sum(values, cards) = 
      case cards of
        [] => values
       | x::xs' => sum(card_value(x) + values, xs')
  in
    sum(0, cards)
  end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) 
  and computesthe score as described. *)
fun score(hcs, goal) = 
  let
      val sum = sum_cards(hcs)
      val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
  in
      if all_same_color(hcs)
      then preliminary_score div 2
      else preliminary_score
  end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list(what the player “does” at each point),
    and an int (the goal) and returns the score at the end of thegame after processing (some or all of) the moves in the move list in order. *)
fun officiate(cards, ms, goal) =
  let
    fun play(cards, moves, holds) = 
      case movess of
        [] => score(holds, goal)
      | m::movess' => case m of
                      Discard d => play(cards, movess', remove_card(holds, d, IllegalMove))
                    | _         => case cards of
                                        []      => score(holds, goal)
                                      | c::cards'  => if sum_cards(holds) > goal
                                                   then score(c::holds, goal)
                                                   else run(cards', movess', c::holds)
  in
    play(cards, ms, [])
  end
