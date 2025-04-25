open ArgumentParser

let print hash name value =
  match Hashtbl.find hash name with
  | Bool b ->
      begin
      match b with
      | true -> print_endline value
      | false -> ()
      end
  | _ -> ()

let echo hash name =
  match Hashtbl.find hash name with
  | String s -> print_endline s
  | _ -> ()

let check res =
  let hash, pos = res in
  print_endline (String.concat " " pos);
  print hash "a-print" "a";
  print hash "b-print" "b";
  print hash "c-print" "c";
  print hash "d-print" "d";
  echo  hash "e-echo";
  print hash "f-print" "f";
  echo  hash "g-echo";
  print hash "h-print" "h";
  print hash "i-print" "i";
  print hash "j-print" "j";
  print hash "k-print" "k";
  print hash "l-print" "l";
  print hash "m-print" "m";
  print hash "n-print" "n";
  print hash "o-print" "o";
  print hash "p-print" "p";
  echo  hash "q-echo"

let () =
  parse_arguments
    ["these"; "are"
    ;"-a"
    ;"-bc"
    ;"hey"
    ;"-de"; "hi"
    ;"--f-print"
    ;"--g-echo"; "hello"
    ;"-hijkl"
    ;"-mnopq"; "heya"
    ;"positional"; "arguments"; "yay"]
    [("a", "a-print", PBool)
    ;("b", "b-print", PBool)
    ;("c", "c-print", PBool)
    ;("d", "d-print", PBool)
    ;("e", "e-echo",  PStr)
    ;("f", "f-print", PBool)
    ;("g", "g-echo",  PStr)
    ;("h", "h-print", PBool)
    ;("i", "i-print", PBool)
    ;("j", "j-print", PBool)
    ;("k", "k-print", PBool)
    ;("l", "l-print", PBool)
    ;("m", "m-print", PBool)
    ;("n", "n-print", PBool)
    ;("o", "o-print", PBool)
    ;("p", "p-print", PBool)
    ;("q", "q-echo",  PStr)]
  |> check
