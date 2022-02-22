open Tezos_crypto
open Yojson.Basic.Util
open Yojson.Basic

let print_json_chest_key c k =
  `Assoc [ ("chest", `String c); ("key", `String k) ]

let hex_to_bytes (input : string) : bytes = Hex.to_bytes (`Hex input)

let print_bytes (b : bytes) : unit = print_endline (Hex.show (Hex.of_bytes b))

let chest_key_to_bytes (ck : Timelock.chest_key) : bytes = Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding ck

let bytes_to_chest_key (b : bytes) : Timelock.chest_key = Data_encoding.Binary.of_bytes_exn Timelock.chest_key_encoding b

let bytes_to_chest (b : bytes) : Timelock.chest = Data_encoding.Binary.of_bytes_exn Timelock.chest_encoding b

let decode_chest_key b = try b |> hex_to_bytes |> bytes_to_chest_key with | e -> print_string "Key: "; raise e

let decode_chest b = try b |> hex_to_bytes |> bytes_to_chest with | e -> print_string "Chest: "; raise e

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

let create_chest_key bchest time =
  let chest = decode_chest bchest in
  let chest_key = Timelock.create_chest_key ~time chest in
  chest_key |> chest_key_to_bytes |> print_bytes

let force bchest time =
  let chest = decode_chest bchest in
  let chest_key = Timelock.create_chest_key ~time chest in
  match Timelock.open_chest chest chest_key ~time with
  | Timelock.Correct v     -> print_bytes v
  | Timelock.Bogus_cipher  -> print_endline "Error: Could not force chest : bogus cipher."
  | Timelock.Bogus_opening -> print_endline "Error: Could not force chest : bogus opening."

let open_ bkey bchest time =
  let chest_key = decode_chest_key bkey in
  let chest = decode_chest bchest in
  match Timelock.open_chest chest chest_key ~time with
  | Timelock.Correct v     -> print_bytes v
  | Timelock.Bogus_cipher  -> print_endline "Error: Could not open chest : bogus cipher."
  | Timelock.Bogus_opening -> print_endline "Error: Could not open chest : bogus opening."

let lock_cmd  = ref false
let force_cmd = ref false
let cck_cmd   = ref false
let open_cmd  = ref false

let data_arg  = ref ""
let chest_arg = ref ""
let key_arg   = ref ""
let time_arg  = ref 0
let anon_args = ref []

let anon a =
  anon_args := a::!anon_args

let speclist = Arg.align [
    "--lock",  Arg.Set lock_cmd, " Generates chest and chest_key values";
    "--force", Arg.Set force_cmd, " Forces chest";
    "--create-chest-key", Arg.Set cck_cmd, " Create chest key from chest";
    "--open",  Arg.Set open_cmd, " Opens chest";

    "--data",  Arg.Set_string data_arg, " Sets data";
    "--chest", Arg.Set_string chest_arg, " Sets chest";
    "--key",   Arg.Set_string key_arg, " Sets chest key";
    "--time",  Arg.Set_int time_arg, " Number of iterations"
  ]

let usage_msg = "timelock-utils [--lock --data <data> | --force --chest <chest> | --create-chest-key --chest <chest> | --open --key <key> --chest <chest>] --time <time>"

let main _ =
  Arg.parse speclist anon usage_msg;
  match !lock_cmd, !force_cmd, !cck_cmd, !open_cmd  with
  | true, false, false,  false -> lock  !data_arg !time_arg
  | false, true, false,  false -> force !chest_arg !time_arg
  | false, false, true,  false -> create_chest_key !chest_arg !time_arg
  | false, false, false, true  -> open_ !key_arg !chest_arg !time_arg
  | _ -> print_endline "Error: Unkwown command."

let _ = main ()
