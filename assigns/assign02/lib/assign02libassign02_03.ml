type dir = North | South | East | West
type path = dir list 
let dist (directions : path) : float = 
  let rec walk (x, y) directions =
    match directions with
    | [] -> (x, y)  
    | North :: rest -> walk (x, y + 1) rest  
    | South :: rest -> walk (x, y - 1) rest  
    | East :: rest -> walk (x + 1, y) rest   
    | West :: rest -> walk (x - 1, y) rest   
  in 
  let (final_x, final_y) = walk (0, 0) directions in 
  let distance = sqrt (float_of_int (final_x * final_x + final_y * final_y)) in
  distance