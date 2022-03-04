(*Tyler Pearson
Jay Elengical
Final Project
CS 476*)

open List

(* Parameters for type: string = table name; bool = is_table_deleted; (string list) list = the actual table*)
type table = Table of bool * string list list 
  
let get_num_categories (tbl : string list list) : int =
  match tbl with
  | hd :: _ -> List.length hd
  | [] -> -1 
  
let rec get_category_index (tbl : string list) (search_item : string) (index : int) : int = 
  match tbl with
  | hd :: tl -> if hd = search_item then index else get_category_index tl search_item (index + 1)
  | [] -> -1
                  
let rec mk_tbl (p : string list) : string list = 
  match p with
  | hd :: tl -> hd :: mk_tbl tl
  | [] -> []  
 
let rec ins_tbl (p : string list list) (lis : string list list) : string list list = 
  match p with
  | hd :: tl -> hd :: ins_tbl tl lis
  | [] -> lis
  
(* Gets the column names from the table *)
let rec get_list_cat (p : string list list) (index : int) (checker : string) : string list = 
  match p with
  | hd :: tl -> if checker <> "null" then
					if (List.nth hd index) = checker then (List.nth hd index) :: get_list_cat tl index checker
					else get_list_cat tl index checker
				else (List.nth hd index) :: get_list_cat tl index checker
  | [] -> []
  
let create_table (params : string list) : table = 
  Table (false, [mk_tbl params])
  
let delete_table (tbl : table) : table = 
  match tbl with
  | Table (false, tab) -> Table (true, tab)
  | _ -> Table (true, [[]])
           
let insert_table (tbl : table) (items : string list list) : table = 
  match tbl with
  | Table (_, lis) -> if get_num_categories lis = List.length items then Table (false, List.rev (ins_tbl items lis))
      else Table (false, lis)
 
(* Steps for searching for item in the table: 
1. For all items in the param list, get indexes of where they are in 
2. With indicies in hand, gather everything we need from each row in the table*) 
let rec slct_tbl (tbl : table) (param : string list) (checker : string) : string list list = 
  match tbl, param with
  | Table (_, lis), hd :: tl -> get_list_cat lis (get_category_index (List.hd lis) hd 0) checker :: (slct_tbl tbl tl checker)
  | _, [] -> [[]]
	
             
let select_table (tbl : table) (param : string list) (checker : string) : string list list =  
  slct_tbl tbl param checker
  

(* Callback registers for the C program*)
(*-------------------------------------*)
(* Callback for creating a table *)
let _ = Callback.register "create_table" create_table
(* Callback for deleting a table *)
let _ = Callback.register "delete_table" delete_table


(* Test 1: Create a table with one row to hold the categories *)
let params = ["Name"; "Age"; "Height"; "Weight"]
let person = create_table params
    
(* Test 2: Successful insertion of a list of parameters into a table *)
let modded_table = insert_table person [["Paul"; "23"; "6'2"; "190 lbs"]];;
    
(* Test 3: Successful deletion of a table (is_delete part of tuple made true) *)

delete_table modded_table;; 

(* Table for test 4: the select statement *)
let complete_table = [["Paul"; "23"; "6'2"; "190 lbs"]; ["Ava"; "33"; "5'7"; "110 lbs."];
                      ["Dennis"; "45"; "5'11"; "237 lbs."]; ["Harold"; "19"; "4'11"; "97.3 lbs."]] 
let db_tbl = insert_table person complete_table;;

(* Test 4: get the names in the table with no clause*)
select_table db_tbl ["Name"] "null";;

(* Test 5: Get the names from the table that equal the WHERE condition "Dennis"*)
(* Should return a list of empty strings and one string "Dennis" within it*)
select_table db_tbl ["Name"] "Dennis";;
