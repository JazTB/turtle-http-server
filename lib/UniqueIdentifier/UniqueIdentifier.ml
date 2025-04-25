let make_uid () =
  let cur_time = Unix.gettimeofday () in
  let seed = Float.to_int (cur_time *. 1000.0) in
  Random.init seed;
  let rec loop = fun x nx y ny nums ->
    match y >= ny with
    | true ->
      begin
        match List.rev nums with
        | _ :: rest -> List.rev rest
        | _ -> []
      end
    | false ->
      begin
        match x >= nx with
        | true -> 
          loop 0 nx (y + 1) ny (nums @ ["-"])
        | false ->
          let num = Random.int 10 
          |> Int.to_string in
          let new_nums = nums @ [num] in
          loop (x + 1) nx y ny new_nums
      end
  in
  let oup = loop 0 2 0 8 [] in
  String.concat "" oup
