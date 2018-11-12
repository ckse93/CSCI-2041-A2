
let curr_list : string list ref = ref [] ;;
let undo_stack : string list list ref = ref [] ;;
let redo_stack : string list list ref = ref [] ;;

(* Reset curr_list, undo_stack, redo_stack to all be empty lists. Used
   only in testing, not in main programs. *)
let reset_all () =
	curr_list  := [];
	undo_stack := [];
	redo_stack := [];
;;

(* curr_list is moved to the top of the undo_Stack. Then curr_list is
   set to the new_list. Empties redo_stack. *)
let set_to_list new_list =
	undo_stack := !curr_list :: !undo_stack; (* X [!curr_list] X = 0 :: [1;2;3;4] *)
	curr_list  := new_list;
	redo_stack := [];
;;

(* Add the given elem to curr_list producing a new_list.  Calls
   set_to_list on result to unable undoing. *)
let add_elem elem =
	let newlist = Sortedlist.insert !curr_list elem in
	set_to_list newlist
;;

(* Remove the given elem from curr_list producing a new list. Calls
   set_to_list on result to unable undoing.  *)
let remove_elem elem =
	let newlist = Sortedlist.remove !curr_list elem in
	set_to_list newlist
;;

(* Merge the param list with the current list. Calls set_to_list on
   the result to enable undoing. *)
let merge_with_list list =
	let newlist = Sortedlist.merge list !curr_list in
	set_to_list newlist
;;

(* If the undo_stack is not empty, undo the last operation. curr_list
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)
let undo () =(*bool*)
	if !undo_stack = [] then false(*see if ref datatype undo_stack is empty, then return false*)
	else
	begin
		redo_stack := !curr_list :: !redo_stack; (*dont use bracket...*)
		curr_list  := List.hd !undo_stack;
		undo_stack := List.tl !undo_stack;
		true
	end
;;

(* If the redo_stack is not empty, redo the last operation. curr_list
   is moved to the undo_stack and the top element of redo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if redo_stack is empty and no changes are made. Operates
   in constant time. *)
let redo () = (*bool*)
	if !redo_stack = [] then false (*same rule as undo*)
	else
	begin (*use of ref + ::*)
		undo_stack := !curr_list :: !undo_stack;
		curr_list  := List.hd !redo_stack;
		redo_stack := List.tl !redo_stack; true
	end
;;
