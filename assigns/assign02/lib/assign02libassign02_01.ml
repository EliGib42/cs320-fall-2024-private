type piece = X | O
type pos = Piece of piece | Blank
type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)
type row_index = 
  | Top 
  | Middle 
  | Bottom
type col_index = 
  | Left 
  | Middle 
  | Right
type pos_index = row_index * col_index
  
let r_one (a, _, _) = a
let r_two (_, b, _) = b
let r_three (_, _, c) = c

let get_pos (board : board) (pos_idx : pos_index) : pos =
  let (row_idx, col_idx) = pos_idx in
  let temp_row = match row_idx with
    | Top -> let (r, _, _) = board in r
    | Middle -> let (_, r, _) = board in r
    | Bottom -> let (_, _, r) = board in r
  in
  match col_idx with
  | Left -> r_one temp_row
  | Middle -> r_two temp_row
  | Right -> r_three temp_row
  