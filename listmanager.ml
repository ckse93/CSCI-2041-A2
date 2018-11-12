(* listmanager.ml : main function to allow manipulation of a sorted
   list of unique elements. Makes use of functions provided in
   Sortedlist, Undolist, and Util. *)

open Printf;;

(* debug printing on/off *)
let debug = ref false;;

(* Help string to be printed for the "help" command. *)
let help_string =
  let lines = [
      "LIST MANAGER";
      "Maintains a sorted list without duplicates";
      "No spaces allowed list data, use _ instead as in Lin_Beifong";
      "Commands:";
      "  help           : print this help message";
      "  quit           : quit the program";
      "  show           : print the current list to the screen";
      "  clear          : set the list to empty, preserves undo history";
      "  add <elem>     : add elem to the list";
      "  remove <elem>  : remove elem from list";
      "  save <file>    : save the current list to named file (not undoable)";
      "  load <file>    : discard the current list, load sorted list in named file (undoable)";
      "  mergein <file> : load the sorted list in the named file and merge with current list (undoable)";
      "  undo           : undo the last operation restoring list to a previous state";
      "  redo           : redo the last undone operation restoring list to a previous state";
    ] in
  String.concat "\n" lines
;;

(* When false, prompts for another command and continues interactive
   loop. When true, ends interactive loop and quits program. *)
let quit_now =
  ref false
;;

(* PROBLEM 4: Execute a single command for the listmanager
   program. Argument tokens is an array of strings at least 1 element
   long.  The 0th element is the command to execute like "add" or
   "clear". Depending on the command, there may be subsequent
   arguments.  Makes use of functions provided in Sortedlist,
   Undolist, and Util a well as values in this module like help_string
   and quit_now. *)
let execute_command tokens =
	match tokens.(0) with
  | "help"  	-> printf "%s\n" help_string
  | "Help"  	-> printf "%s\n" help_string
	| "quit"  	-> quit_now := true
  | "Quit"  	-> quit_now := true
	| "show"  	-> printf "--BEG LIST--\n";
                 Sortedlist.print (!Undolist.curr_list);
				         printf "--END LIST--\n"
  | "Show"  	-> printf "--BEG LIST--\n";
                 Sortedlist.print (!Undolist.curr_list);
               	 printf "--END LIST--\n"
	| "clear" 	-> Undolist.set_to_list []
  | "Clear" 	-> Undolist.set_to_list []
	| "add"	   	-> Undolist.add_elem (tokens.(1))
  | "Add"	   	-> Undolist.add_elem (tokens.(1))
	| "remove"	-> Undolist.remove_elem (tokens.(1))
  | "Remove"	-> Undolist.remove_elem (tokens.(1))
	| "save"	  -> Util.strlist_to_file (!Undolist.curr_list) (tokens.(1))
  | "Save"	  -> Util.strlist_to_file (!Undolist.curr_list) (tokens.(1))
	| "load"	  -> let cur = Util.strlist_from_file (tokens.(1)) in
				         Undolist.set_to_list (cur)
  | "Load"	  -> let cur = Util.strlist_from_file (tokens.(1)) in
               	 Undolist.set_to_list (cur)
	| "mergein" -> let cur = Util.strlist_from_file (tokens.(1)) in
				         Undolist.merge_with_list (cur)
  | "Mergein" -> let cur = Util.strlist_from_file (tokens.(1)) in
                 Undolist.merge_with_list (cur)
	| "undo"    -> let undo = Undolist.undo() in
				          if undo = false then printf "WARNING: undo list empty, no changes made\n"
  | "Undo"    -> let undo = Undolist.undo() in
                	if undo = false then printf "WARNING: undo list empty, no changes made\n"
	| "redo" 	  -> let redo = Undolist.redo() in
				          if redo = false then printf "WARNING: redo list empty, no changes made\n"
  | "Redo" 	  -> let redo = Undolist.redo() in
                	if redo = false then printf "WARNING: redo list empty, no changes made\n"
	| _ 		    -> printf "unknown command '%s'\n" tokens.(0) (*other case check in*)
;;

(**********************************************************************************)
(* Code beyond this point should not require modification though it
   may be interesting to examine. *)


let echo  = ref false;;         (* command echoing on/off  *)
let prompt = "LM> ";;           (* prompt for command line *)

(* Options accepted by the program *)
let options = Arg.([
  ("-echo",  Set(echo),  "Turn on command echoing (default: off)");
  ("-debug", Set(debug), "Turn on debug printing  (default: on)");
]);;

(* Do nothing with extra command line arguments *)
let handle_extra_args arg = ();;

(* Simple usage message for Arg.parse *)
let usage = sprintf "usage: %s [options]" Sys.argv.(0);;

(* main routine *)
let _ =
  Arg.parse options handle_extra_args usage;    (* parse command line options *)
  begin try
      while !quit_now != true do                (* loop until quit command is issued *)
        printf "%s" prompt;                     (* print prompt *)
        let line = read_line () in              (* read a line of input from stdin *)
        if !echo then                           (* if echoing is on, print the line *)
          printf "%s\n" line;
        let tokens =                            (* split line into tokens on spaces *)
          Array.of_list (Str.split (Str.regexp " +") line) in
        let ntok = Array.length tokens in
        if !debug then                          (* possibly print debuggin info on tokens *)
          begin
            printf "'%s' has %d tokens\n" line ntok;
            for i=0 to ntok-1 do
              printf "%d : %s\n" i tokens.(i);
            done
          end;
        if ntok>0 then
          try execute_command tokens;           (* execute a command *)
          with e ->                             (* out of bounds access, file not found, etc. doesn't kill program *)
            let excstring = Printexc.to_string e in
            printf "Error with '%s': %s\n" line excstring;
      done;
    with
    | End_of_file -> ()                         (* end of input reached *)
  end;
  printf "\nList managed!\n";
;;
