type t = Sqlite3.db

let assert_ok condition message check =
  if check != condition then
    failwith message

let create file_name =
  let open Sqlite3 in
  let self = db_open file_name in
  "CREATE TABLE IF NOT EXISTS turtles ( \
    uid STRING UNIQUE NOT NULL, \
    instructions STRING NOT NULL \
  )"
  |> exec self
  |> Rc.is_success
  |> assert_ok true "Failed to create DB";
  Printf.sprintf
    "INSERT OR IGNORE INTO turtles \
       (uid, instructions) \
       VALUES ('%s', '%s')"
    "all" "[]"
  |> exec self
  |> Rc.is_success
  |> assert_ok true "Failed insert all into DB";
  self

let close self =
  Sqlite3.db_close self
  |> assert_ok true "Failed to close DB"

let get_all_uids self =
  let open Sqlite3 in
  let uids = ref [] in
  let cb = fun row ->
    match row.(0) with
    | Some v ->
      uids := !uids @ [v]
    | None -> ()
  in
  "SELECT uid, instructions FROM turtles"
  |> exec_no_headers self ~cb:cb
  |> Rc.is_success
  |> assert_ok true "Failed to get from DB";
  !uids

let add_new_uid self uid =
  let open Sqlite3 in
  Printf.sprintf
    "INSERT INTO turtles \
      (uid, instructions) \
      VALUES ('%s','%s')"
    uid "[]"
  |> exec self
  |> Rc.is_success
  |> assert_ok true "Failed to insert into DB"

let add_instruction self instruction uid =
  let open Sqlite3 in
  let instructions_json = ref "[]" in
  let cb = fun r ->
    match r.(0) with
    | Some v -> instructions_json := v
    | None -> ()
  in
  Printf.sprintf
    "SELECT instructions FROM turtles WHERE \
      uid = '%s'"
    uid
  |> exec_no_headers self ~cb:cb
  |> Rc.is_success
  |> assert_ok true "Failed to get from DB";

  let open Yojson in
  let old_messages =
    Basic.from_string !instructions_json
    |> Basic.Util.to_list
    |> List.map Basic.Util.to_string
    |> List.map Basic.from_string
    |> List.map (fun json: Messages.t ->
      let open Yojson.Basic.Util in
      let msg =
        json
        |> member "msg"
        |> to_string
      in
      let args =
        json
        |> member "args"
        |> to_list
        |> List.map to_string
      in
      {message = msg; arguments = args})
  in
  let new_messages =
    old_messages @ [instruction]
  in
  let new_json =
    let jsonified =
      List.map Messages.to_json new_messages
      |> List.map (fun v ->
        `String v)
    in
    `List jsonified 
    |> to_string
  in
  (*print_endline new_json;*)
  Printf.sprintf
    "UPDATE turtles SET instructions = '%s' \
      WHERE uid = '%s'"
    new_json uid
  |> exec self
  |> Rc.is_success
  |> assert_ok true "Failed to update DB"

let pop_instruction self uid =
  let returned_value =
    Messages.create "wait" ["2"]
    |> ref
  in
  let open Sqlite3 in
  let instructions_json = ref "[]" in
  let cb = fun r ->
    match r.(0) with
    | Some v -> instructions_json := v
    | None -> ()
  in
  Printf.sprintf
    "SELECT instructions FROM turtles WHERE \
      uid = '%s'"
    uid
  |> exec_no_headers self ~cb:cb
  |> Rc.is_success
  |> assert_ok true "Failed to get from DB";

  let open Yojson in
  let old_messages =
    Basic.from_string !instructions_json
    |> Basic.Util.to_list
    |> List.map Basic.Util.to_string
    |> List.map Basic.from_string
    |> List.map (fun json: Messages.t ->
      let open Yojson.Basic.Util in
      let msg =
        json
        |> member "msg"
        |> to_string
      in
      let args =
        json
        |> member "args"
        |> to_list
        |> List.map to_string
      in
      {message = msg; arguments = args})
  in
  let new_messages = match old_messages with
  | [] -> []
  | v :: rest ->
    (*print_endline (Messages.to_json v);*)
    returned_value := v;
    rest
  in
  let new_json =
    let jsonified =
      List.map Messages.to_json new_messages
      |> List.map (fun v -> `String v)
    in
    `List jsonified 
    |> to_string
  in
  Printf.sprintf
    "UPDATE turtles SET instructions = '%s' \
      WHERE uid = '%s'"
    new_json uid
  |> exec self
  |> Rc.is_success
  |> assert_ok true "Failed to update DB";
  !returned_value

let new_uid self =
  let open UniqueIdentifier in
  let uid = make_uid () in
  let rec loop = fun uid ->
    let uid_list = get_all_uids self in
    match List.exists ((=) uid) uid_list with
    | true -> loop (make_uid ())
    | false -> uid
  in
  loop uid
