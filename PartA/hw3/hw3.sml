
(* Author: Mustafa Jamal *)
(* Coursera Programming Languages, Homework 3 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. only_capitals that takes a string list and returns a string list that has 
	onlythe strings in the argument that start with an uppercase letter. *)
fun only_capitals sList =
		List.filter (fn str_ => Char.isUpper(String.sub(str_, 0))) sList

(* 2. longest_string1 that takes a string list and returns the longest string in thelist. *)
fun longest_string1 sList =
		foldl (fn (str_, acc) => if (String.size str_ > String.size acc) then str_ else acc) "" sList

(* 3. Write a function longest_string2 that is exactly like longest_string1 
	except in the case of tiesit returns the string closest to the end of the list. *)
fun longest_string2 sList =
		foldl (fn (str_, acc) => if (String.size str_ < String.size acc) then acc else str_)
			  "" sList

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4 *)
fun longset_string_helper compare_func str_lst =
  foldl (fn (x,y) => if compare_func(String.size x,String.size y) then x else y)
        ""
        str_lst

val longest_string3 = longset_string_helper (fn (x,y) => x > y)

val longest_string4 = longset_string_helper (fn (x,y) => x >= y)




(* 5. longest_capitalized that takes a string list and returns the longest string *)
val longest_capitalized = longest_string1 o only_capitals

(* 6. rev_string that takes a string and returns the string that is the same characters inreverse order. *)
val rev_string = String.implode o rev o String.explode

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f lst =
		case lst of
			[] => raise NoAnswer
		  | (x::xs') => 	case f x of
		  						NONE => first_answer f xs'
							  | SOME x => x

(* 8. all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option. *)
fun all_answers f lst =
		let
		  	fun aux lst acc = 
			  	case lst of
			   		[] => SOME acc
				  | (x::lst') => case f x of
				  					NONE => NONE
			  					  | SOME ans => helper lst' (acc@ans)
		in
		  aux lst []
		end

(* 9. a *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* 9. b *)
fun count_wild_and_variable_lengths p =
  g (fn () => 1) String.size p
(* 9. c *)
fun count_some_var (s, p) = 
g (fn () => 0) (fn str => if str = s then 1 else 0) p

(* 10. check_pat that takes a pattern and returns true
	if and only if all the variablesappearing in the pattern are distinct from each other. *)
fun check_pat p = 
		let
		  	fun all_strings p =
			  	case p of 
				  	Variable x                    => [x]
				  | TupleP ps => foldl (fn (p, i) => all_strings p@i) [] ps
				  | ConstructorP (_, p)           => all_strings p
				  | _                             => []
			fun no_repeates strs = 
				case strs of
					[]       => true
				  | (s::strs') => not (List.exists (fn x => s = x) strs') andalso no_repeates strs'
		in
			no_repeates (all_strings p)
		end

(* 11. match that takes a valu * pattern and returns a (string * valu) list option. *)
fun match (v, p) =
  	case (v, p) of
      (_, Wildcard) => SOME []
    | (sv, Variable sp) => SOME [(sp,sv)]
    | (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple tv, TupleP tp) => if List.length tv = List.length tp
                               then all_answers match (ListPair.zip(tv, tp))
                               else NONE
    | (Constructor (s1,cv), ConstructorP (s2,cp)) => if s1 = s2
                                                     then match (cv,cp)
                                                     else NONE
    | (_, _) => NONE

(* 12. first_match that takes a value and a list of patterns and returns a(string * valu) list option *)
fun first_match v p_lst = 
		SOME (first_answer (fn p => match (v, p)) p_lst)
		handle NoAnswer => NONE
