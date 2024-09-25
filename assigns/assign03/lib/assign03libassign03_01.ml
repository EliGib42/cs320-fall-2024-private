let mk_unique_keys alst =
  (* I created a helper function that searches the input list to find if the key exists or not.If a key does exist, 
  the values are added. If not, the new pair is added *)
  let rec updater (key, value) pairs =
    match pairs with
    | [] -> [(key, value)]  (* No matching key in this case*)
    | (k, v) :: rest when k = key -> (k, v + value) :: rest  (* Add values if there is a matching key in this case *)
    | pair :: rest -> pair :: updater (key, value) rest  
  in 
  (* Main loop. Goes through the list andprocesses each while updating the result as it goes *)
  let rec process_list lst result =
    match lst with
    | [] -> result  
    | (key, value) :: rest -> 
        process_list rest (updater (key, value) result)
  in 
  process_list alst []