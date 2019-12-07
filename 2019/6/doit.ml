let _ =
  let rec read_one l =
    try
      match read_line () |> String.split_on_char ')' with
      | [a;b] ->
         read_one ((b,a) :: l )
      | _ -> failwith "Invalid input format"
    with End_of_file -> l in
  let rec orbits b l n =
    try
      let a = List.assoc b l in
      1 + orbits a l n
    with Not_found -> n in
  let rec path_to b l other_path =
    try
      if List.mem b other_path
      then
        []
      else
        let a = List.assoc b l in
        b :: path_to a l other_path
    with Not_found -> [] in
  let l = read_one [] in
  let _ = List.fold_left (fun n (b,_) -> n + orbits b l 0) 0 l |>
            Printf.printf "Result: %d\n" in
  let pryou = path_to "YOU" l []
  and prsan = path_to "SAN" l [] in
  let psan = path_to "SAN" l pryou
  and pyou = path_to "YOU" l prsan in
  Printf.printf "YOU to SAN:%d\n" (List.length pyou + List.length psan - 2)
