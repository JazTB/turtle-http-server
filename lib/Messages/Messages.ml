type t = {
  message:   string;
  arguments: string list
}

type message_t =
  | Wait       of int
  | RefuelIf   of int
  | GrantUID   of string
  | Move       of string
  | Turn       of string
  | Look       of string
  | Dig        of string
  | FuelCount
  | Refuel
  | Exit
  | GetPosInfo
  | Sequence   of message_t list

let create message arguments =
  {message = message; arguments = arguments}

let to_json self =
  let open Yojson in
  let f = fun v ->
    `String v
  in
  let args = List.map f self.arguments in
  `Assoc [
    ("msg",  `String self.message);
    ("args", `List args)
  ] |> to_string

let from_json str =
  let open Yojson in
  let json = Basic.from_string str in
  let open Yojson.Basic.Util in
  let message =
    json
    |> member "message"
    |> to_string in
  let arguments =
    json
    |> member "arguments"
    |> to_list
    |> List.map to_string
  in
  {message = message; arguments = arguments}

let rec create_from message_type =
  match message_type with
  | Wait time ->
    create "wait" [Int.to_string time]
  | RefuelIf less_than ->
    create "refuel-if" [Int.to_string less_than]
  | GrantUID uid ->
    create "grant-uid" [uid]
  | Move direction ->
    create "move" [direction]
  | Turn direction ->
    create "turn" [direction]
  | Look direction ->
    create "look" [direction]
  | Dig direction ->
    create "dig" [direction]
  | FuelCount ->
    create "fuel-count" []
  | Refuel ->
    create "refuel" []
  | Exit ->
    create "exit" []
  | GetPosInfo ->
    create "pos-info" []
  | Sequence seq ->
    let t_list = List.map create_from seq in
    let json_list = List.map to_json t_list in
    create "sequence" json_list
