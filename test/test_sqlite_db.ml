let assert_ok condition message check =
  if check != condition then
    failwith message

let cmp_messages
  (one: Messages.t)
  (two: Messages.t): bool =
  let rec iter_arguments = fun a b ->
    match a with
    | [] -> true
    | ax::ay ->
      match b with
      | [] -> false (* unexpected? *)
      | bx::by ->
        match ax = bx with
        | false -> false
        | true -> iter_arguments ay by
  in
  match one.message = two.message with
  | false -> false
  | true ->
    match
      (List.length one.arguments) =
      (List.length two.arguments)
    with
    | false -> false
    | true ->
      iter_arguments one.arguments two.arguments

let () =
  let db =
    SqliteHandler.create ":memory:"
  in

  let one = SqliteHandler.new_uid db in
  SqliteHandler.add_new_uid db one;
  Printf.printf "One: %s\n" one;
  let two = SqliteHandler.new_uid db in
  SqliteHandler.add_new_uid db two;
  Printf.printf "Two: %s\n" two;

  print_endline "Adding 1";
  let ins = Messages.create "wait" ["1"] in
  SqliteHandler.add_instruction db ins one;
  print_endline "Adding 2";
  let ins = Messages.create "wait" ["2"] in
  SqliteHandler.add_instruction db ins one;
  print_endline "Adding 3";
  let ins = Messages.create "wait" ["3"] in
  SqliteHandler.add_instruction db ins one;

  SqliteHandler.pop_instruction db one
  |> cmp_messages (Messages.create "wait" ["1"])
  |> assert_ok true "Expected wait 1";
  SqliteHandler.pop_instruction db one
  |> cmp_messages (Messages.create "wait" ["2"])
  |> assert_ok true "Expected wait 2";
  SqliteHandler.pop_instruction db one
  |> cmp_messages (Messages.create "wait" ["3"])
  |> assert_ok true "Expected wait 3";

  print_endline "Adding 4";
  let ins = Messages.create "wait" ["4"] in
  SqliteHandler.add_instruction db ins two;
  print_endline "Adding 5";
  let ins = Messages.create "wait" ["5"] in
  SqliteHandler.add_instruction db ins two;
  print_endline "Adding 6";
  let ins = Messages.create "wait" ["6"] in
  SqliteHandler.add_instruction db ins two;

  SqliteHandler.pop_instruction db two
  |> cmp_messages (Messages.create "wait" ["4"])
  |> assert_ok true "Expected wait 4";
  SqliteHandler.pop_instruction db two
  |> cmp_messages (Messages.create "wait" ["5"])
  |> assert_ok true "Expected wait 5";
  SqliteHandler.pop_instruction db two
  |> cmp_messages (Messages.create "wait" ["6"])
  |> assert_ok true "Expected wait 6";
   
  SqliteHandler.close db
