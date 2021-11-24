open List

(* Parameters for type: string = table name; bool = is_table_deleted; (string list) list = the actual table*)
type table = Table of bool * (string list) list 
  
let get_num_categories (tbl : (string list) list) : int =
  match tbl with
  | hd :: _ -> List.length hd
  | [] -> -1
                  
let rec mk_tbl (p : string list) : string list = 
  match p with
  | hd :: tl -> hd :: (mk_tbl tl) 
  | [] -> []                              

let create_table (params : string list) : table = 
  Table (false, [mk_tbl params])
        
(* Gets the acutal table from the table structure *)
let get_table (tbl : table) : ((string list) list) = 
  match tbl with
  | Table (_, l) -> l
  | _ -> [[]] 
          
let delete_table (tbl : table) : table = 
  match tbl with
  | Table (false, tab) -> Table (true, tab)
  | _ -> Table (true, [[]])
           
let insert_table (tbl : table) (items : string list) : table= 
  match tbl with
  | Table (_, lis) -> if get_num_categories lis = List.length items then Table (false, List.rev (items :: lis))
      else Table (false, lis)
  | _ -> Table (true, [[]])
           
  
(* Test 1: Create a table with one row to hold the categories *)
let params = ["Name"; "Age"; "Height"; "Weight"]
let person = create_table params
    
(* Test 2: Successful insertion of a list of parameters into a table *)
let modded_table = insert_table person ["Paul"; "23"; "6'2"; "190 lbs"];;
    
(* Test 3: Successful deletion of a table (is_delete part of tuple made true) *)

delete_table modded_table;; 
  