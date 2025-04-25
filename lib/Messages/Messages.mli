type t = {
  message: string;
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

val create: string -> string list -> t
val to_json: t -> string
val from_json: string -> t
val create_from: message_t -> t
