open Tezos_crypto
open Yojson.Basic.Util
open Yojson.Basic

let print_json_chest_key c k =
  `Assoc [ ("chest", `String c); ("key", `String k) ]

(* "\x05\x00\x80\x89\x0f" *)

let hex_to_bytes (input : string) : bytes = Hex.to_bytes (`Hex input)

let print_bytes (b : bytes) : unit = print_endline (Hex.show (Hex.of_bytes b))

let bytes_to_chest (b : bytes) : Timelock.chest = Data_encoding.Binary.of_bytes_exn Timelock.chest_encoding b

let lock pl time =
  let payload = hex_to_bytes pl in
  let (chest, chest_key) = Timelock.create_chest_and_chest_key ~payload ~time in
  let chest_key_bytes =
    chest_key
    |> Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding
    |> Hex.of_bytes
    |> Hex.show
  in
  let chest_bytes =
    chest
    |> Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding
    |> Hex.of_bytes
    |> Hex.show
  in
  print_endline (pretty_to_string (print_json_chest_key chest_bytes chest_key_bytes))

let force bchest time =
  let chest = bchest |> hex_to_bytes |> bytes_to_chest in
  let chest_key = Timelock.create_chest_key ~time chest in
  match Timelock.open_chest chest chest_key ~time with
  | Timelock.Correct v     -> print_bytes v
  | Timelock.Bogus_cipher  -> print_endline "Could not force chest : bogus cipher."
  | Timelock.Bogus_opening -> print_endline "Could not force chest : bogus opening."


let lock_arg = ref false
let force_arg = ref false

let data_arg = ref ""
let chest_arg = ref ""
let time_arg = ref 0
let anon_args = ref []

let anon a =
  anon_args := a::!anon_args

let speclist = Arg.align [
    "--lock",  Arg.Set lock_arg, "Generates chest and chest_key values";
    "--force", Arg.Set force_arg, "Forces chest";
    "--data",  Arg.Set_string data_arg, "Set data";
    "--chest", Arg.Set_string chest_arg, "Set chest";
    "--time",  Arg.Set_int time_arg, "Number of iterations"
  ]

let usage_msg = "timelock-utils [lock <value> | force <chest>] -time <time>"

(* let usage_msg =
 "timelock-utils\
   command
     lock <data> <time>
     force <chest> <time>
 " *)

let main _ =
  Arg.parse speclist anon usage_msg;
  match !lock_arg, !force_arg with
  | true, false  -> lock !data_arg !time_arg
  | false, true -> force !chest_arg !time_arg
  | _ -> assert false

    (* print_endline !value_arg;
       print_endline !chest_arg;
       print_endline (string_of_int !time_arg); *)
    (* if (compare !value_arg "") <> 0 then begin lock !value_arg !time_arg end;
    if (compare !chest_arg "") <> 0 then begin force !chest_arg !time_arg end *)
(* TODO : check out to go from string to chest to force*)

let _ = main ()
