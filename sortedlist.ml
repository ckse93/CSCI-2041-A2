(* sortedlist.ml : Provides operations for sorted lists of any type. *)
open Printf;;
\
*)
let rec insert lst elem =
  match lst with
    | []                            -> [elem]
    | head :: tail when elem = head -> lst
    | head :: tail when elem < head -> elem :: lst
    | head :: tail when elem > head -> head :: insert tail elem
;;
(* val insert : 'a list -> 'a -> 'a list
   PROBLEM 1: Insert elem into list which is sorted.  The insertion
   preserves the sorted order of the list.  No duplicates are allowed
   in the list: if elem is equal to an element of list, the resulting
   list is identical to the original list. Uses pattern matching, not
   tail recursive. Runs in linear time on length of list.

  REPL EXAMPLES
  # insert [1;3;5;7] 8;;
  - : int list = [1; 3; 5; 7; 8]
  # insert [1;3;5;7] 2;;
  - : int list = [1; 2; 3; 5; 7]
  # insert [1;3;5;7] 5;;
  - : int list = [1; 3; 5; 7]
  # insert ["b";"d";"f"] "a";;
  - : string list = ["a"; "b"; "d"; "f"]
  # insert ["b";"d";"f"] "g";;
  - : string list = ["b"; "d"; "f"; "g"]
  # insert ["b";"d";"f"] "b";;
  - : string list = ["b"; "d"; "f"]
  # insert [] "g";;
  - : string list = ["g"]
*)



let rec remove lst elem =
  match lst with
    | head :: tail
      when elem = head -> tail
    | head :: tail
      when elem > head -> head :: remove tail elem
    | head :: tail
      when elem < head -> lst
    | _ -> lst
 ;;
 (* val remove : 'a list -> 'a -> 'a list
    PROBLEM 1: Create a new list with elem removed from the parameter
    list. If elem is not present in list, the result is identical to
    the original list. Uses pattern matching, not tail recursive.
    Runs in linear time on length of list.

    REPL EXAMPLES
    # remove [1;3;5;7] 1;;
    - : int list = [3; 5; 7]
    # remove [1;3;5;7] 5;;
    - : int list = [1; 3; 7]
    # remove [1;3;5;7] 6;;
    - : int list = [1; 3; 5; 7]
    # remove ["b";"d";"f"] "b";;
    - : string list = ["d"; "f"]
    # remove ["b";"d";"f"] "z";;
    - : string list = ["b"; "d"; "f"]
 *)


let rec print strlist =
  match strlist with
  | head :: tail -> printf ("%s \n") head; (*and then, do this ->*)
  print tail
  | head :: [] -> printf ("%s \n") head
  | [] -> printf("")
 ;;
 (* val print_strlist : string list -> unit
   PROBLEM 1: Print all elements of a string list to standard
   output. Makes use of standard printing functions such as
   print_endline or printf to print. Uses pattern matching. This
   function IS tail recursive. Runs in linear time on length of list.

   REPL EXAMPLES
   # print ["apple";"orange";"banana"];;
   apple
   orange
   banana
   - : unit = ()
   # print ["grape";"pear"];;
   grape
   pear
   - : unit = ()
*)


let rec merge lista listb =
	match lista, listb with
	| [] 		, [] 						-> []
	| []		, (bhead :: btail) 	-> listb
	| (ahead :: atail), [] -> lista
	| (ahead :: atail), (bhead :: btail)  when ahead = bhead  -> ahead :: merge atail btail
	| (ahead :: atail), (bhead :: btail)  when ahead > bhead  -> bhead :: merge lista btail
	| (ahead :: atail), (bhead :: btail) -> ahead :: merge atail listb
;;
(* val merge : 'a list -> 'a list -> 'a list
   PROBLEM 2: Merge two sorted lists: lista and listb.  Elemetns that
   appear in both lists appear only once in the result.  Operates in
   linear time on the length of lists: does not do repeated
   insertion. Not tail recursive. May use pattern matching OR if/else
   clauses. Runs in linear time on combined length of lists.

   REPL EXAMPLES
   # merge [] [2;4;6];;
   - : int list = [2; 4; 6]
   # merge [1;3;5] [2;4;6];;
   - : int list = [1; 2; 3; 4; 5; 6]
   # merge [1;3;5] [];;
   - : int list = [1; 3; 5]
   # merge [1;3;5] [2;3;4;6;8];;
   - : int list = [1; 2; 3; 4; 5; 6; 8]
   # merge ["a";"c";"e"] ["b";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
   # merge ["a";"c";"e"] ["b";"c";"d";"e"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
*)
