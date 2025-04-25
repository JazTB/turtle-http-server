open Lwt
open Cohttp
open Cohttp_lwt_unix

let parse_uri uri =
  (* uri looks like //127.0.0.1:8080/<stuff> *)
  let re = Str.regexp "^//[^/]*?/\\(.*\\)$" in
  try
    Str.search_forward re uri 0 |> ignore;
    Str.matched_group 1 uri
  with _ ->
    ""

type good_request_t =
  | Ok of string
  | Bad of string
exception NoUIDProvided of unit
let handle_channel meth body db mutex =
  match meth with
  | "GET" ->
    Lwt_io.printf
      "Unknown GET message!\n"
    |> ignore;
    Lwt_io.flush Lwt_io.stdout |> ignore;
    Bad "Unknown GET request!"
  | "POST" -> 
    let msg = Messages.from_json body in
    begin
      match (msg.message, msg.arguments) with
      | "get-uid", _ ->
        Lwt_mutex.lock mutex |> ignore;
        let new_uid = SqliteHandler.new_uid db in
        Lwt_io.printf
          "Granting UID: %s\n"
          new_uid
        |> ignore;
        Lwt_io.flush Lwt_io.stdout |> ignore;
        Lwt_mutex.unlock mutex;
        Ok (Messages.GrantUID new_uid
        |> Messages.create_from
        |> Messages.to_json)
      | "instructions", args ->
        let uid = match args with
        | [] -> raise (NoUIDProvided ())
        | v::_ -> v
        in
        Lwt_io.printf
          "Serving instructions to %s\n"
          uid
        |> ignore;
        Lwt_io.flush Lwt_io.stdout |> ignore;
        Lwt_mutex.lock mutex |> ignore;
        let inst =
          SqliteHandler.pop_instruction db uid
        in
        Lwt_mutex.unlock mutex;
        Ok (Messages.to_json inst)
      | "looked", _ ->
        (* TODO *)
        Lwt_io.printf
          "Looking not implemented\n"
        |> ignore;
        Lwt_io.flush Lwt_io.stdout |> ignore;
        Ok (Messages.Wait 2000
        |> Messages.create_from
        |> Messages.to_json)
      | "fuelcount", _ ->
        (* TODO *)
        Lwt_io.printf
          "FuelCount not implemented\n"
        |> ignore;
        Lwt_io.flush Lwt_io.stdout |> ignore;
        Ok (Messages.Wait 2000
        |> Messages.create_from
        |> Messages.to_json)
      | _, _ ->
        Lwt_io.printf
        "Unknown POST message!\n"
        |> ignore;
        Lwt_io.flush Lwt_io.stdout |> ignore;
        Bad "Unknown POST request!"
    end
  | _ -> Bad "Unknown Method!"

type instructions_t = {
  instructions: Messages.t;
  uid: string
}
let parse_inst body =
  let open Yojson in
  let json = Basic.from_string body in
  let open Yojson.Basic.Util in
  let instructions =
    json
    |> member "instructions"
    |> to_string
    |> Messages.from_json
  in
  let uid =
    json
    |> member "uid"
    |> to_string
  in
  {instructions; uid}

let handle_request uri meth body db mutex =
  let path = parse_uri uri in
  match path with
  | "channel" ->
    handle_channel meth body db mutex
  | "queue" ->
    let inst = parse_inst body in
    Lwt_mutex.lock mutex |> ignore;
    SqliteHandler.add_instruction
      db
      inst.instructions
      inst.uid;
    Lwt_mutex.unlock mutex;
    Ok ""
  | v ->
    Lwt_io.printf
      "Unknown path: %s\n"
      v
    |> ignore;
    Lwt_io.flush Lwt_io.stdout |> ignore;
    Bad (Printf.sprintf
      "Unknown path: %s"
      v)

let server port db mutex =
  let callback _conn req body =
    let uri =
      Request.uri req |> Uri.to_string
    in
    let meth =
      Request.meth req |> Code.string_of_method
    in
    begin
      Cohttp_lwt.Body.to_string body
      >|= fun body ->
        handle_request uri meth body db mutex
    end
    >>= fun body ->
      match body with
      | Bad v ->
        Server.respond_error
          ~status:`Bad_request
          ~body:v
          ()
      | Ok v ->
        Server.respond_string
          ~status:`OK
          ~body:v ()
  in
  let mode = `TCP (`Port port) in
  Server.make ~callback ()
  |> Server.create ~mode

let () =
  let flags, _ =
    ArgumentParser.parse_arguments
      (Array.to_list Sys.argv)
      [("p", "port", PStr)]
  in
  let port = match (Hashtbl.find_opt flags "port") with
  | None -> 8080
  | Some v -> match v with 
    | String v -> int_of_string v
    | Undefined | Bool _ -> 8080
  in
  let db =
    SqliteHandler.create "./turtles.sqlite"
  in
  let mutex = Lwt_mutex.create () in
  Lwt_main.run (server port db mutex);
  SqliteHandler.close db
