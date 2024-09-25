type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

(* Wrote a helper function to take a list and recursively turn it into a list of rows with each having num_cols elements *)
let rec make_rows (all_entries : float list) (num_cols : int) : float list list = 
  match all_entries with
  | [] -> [] 
  | _ ->
      let current_row = List.take num_cols all_entries in
      let remaining_entries = List.drop num_cols all_entries in
      current_row :: make_rows remaining_entries num_cols

let mk_matrix (entries : float list) ((num_rows, num_cols) : int * int) : matrix = 
  let rows_list = make_rows entries num_cols in 
  { entries = rows_list; rows = num_rows; cols = num_cols }