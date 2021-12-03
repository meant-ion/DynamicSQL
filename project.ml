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
  
let rec get_list_cat (p : string list list) (index : int) : string list = 
  match p with
  | hd :: tl -> (List.nth hd index) :: get_list_cat tl index
  | [] -> []
  
let create_table (params : string list) : table = 
  Table (false, [mk_tbl params])
        
(* Gets the acutal table from the table structure *)
let get_table (tbl : table) : string list list = 
  match tbl with
  | Table (_, l) -> l
  | _ -> [[]] 
          
let delete_table (tbl : table) : table = 
  match tbl with
  | Table (false, tab) -> Table (true, tab)
  | _ -> Table (true, [[]])
           
let insert_table (tbl : table) (items : string list list) : table= 
  match tbl with
  | Table (_, lis) -> if get_num_categories lis = List.length items then Table (false, List.rev (ins_tbl items lis))
      else Table (false, lis)
  | _ -> Table (true, [[]])
 
(* Steps for searching for item in the table: 
1. For all items in the param list, get indexes of where they are in 
2. With indicies in hand, gather everything we need from each row in the table*) 
let rec slct_tbl (tbl : table) (param : string list) : string list list = 
  match tbl, param with
  | Table (_, lis), hd :: tl -> get_list_cat lis (get_category_index (List.hd lis) hd 0) :: (slct_tbl tbl tl)
  | where_clause
  | _, [] -> [[]]
  
(*let rec where_clause (tbl: table) (param : string list) = 
	match tbl, param with
	| Table (_,lis), hd :: tl -> if val get_category_index = exist then true
	| else false*)
	
(*let rec where_clause_2 (tbl : table) (param : string list) (checker : index) string list = 
	match tbl, param, and checker
	| Table (_,lis), hd :: tl -> if val get_category_index = exist then true
	| else false
	| _ -> [[]]*)
	
(*let rec where (tbl : table) (param : string list) index = 
	match tbl, param, index with
	| Table (_, lis) hd :: tl -> if val index of get_category_index exist then true
	| else false*)
	
             
let select_table (tbl : table) (param : string list) : string list list = 
  slct_tbl tbl param
  
  
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

select_table db_tbl ["Name"]
  
