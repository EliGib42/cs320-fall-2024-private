let gen_fib start_vals n = 
  (* Realized that the index could be a value our of bounds which would make the functions not work,
  added this function to check quickly *)
  let oooe_checker listy index =
    if index < 0 || index >= List.length listy then
      failwith "Index out of bounds"
    else
      List.nth listy index
  in 
  let rec tail_recursion_helperfunct listy lengthy =
    match listy with
    | [] -> 0  
    | _ when List.length listy <= lengthy -> List.fold_left (+) 0 listy  
    | _ -> tail_recursion_helperfunct (List.tl listy) lengthy  
  in 
  let rec get_fib current index =
    if index < List.length start_vals then
      List.nth start_vals index  
    else
      let next_val = tail_recursion_helperfunct current (List.length start_vals) in
      get_fib(next_val :: current) (index + 1)  
  in 
  get_fib(List.rev start_vals) 