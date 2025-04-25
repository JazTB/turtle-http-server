type t (* db *)

val create: string -> t
val close: t -> unit
val get_all_uids: t -> string list
val add_new_uid: t -> string -> unit
val add_instruction:
  t -> Messages.t -> string -> unit
val pop_instruction: t -> string -> Messages.t
val new_uid: t -> string
