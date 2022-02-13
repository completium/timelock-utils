open Tezos_crypto
open Yojson.Basic.Util
open Yojson.Basic

let usage_msg = "timelock-utils [-lock <value>|-force <chest>] -time <time>"

let value_arg = ref ""
let chest_arg = ref ""
let time_arg = ref 0
let anon_args = ref []

let anon a =
  anon_args := a::!anon_args

let speclist = [
  ("-lock", Arg.Set_string value_arg, "Generates chest and chest_key values");
  ("-force", Arg.Set_string chest_arg, "Force open a chest");
  ("-time", Arg.Set_int time_arg, "Number of iterations")
]

let print_json_chest_key c k =
  `Assoc [ ("chest", `String c); ("key", `String k) ]

(* "\x05\x00\x80\x89\x0f" *)

let lock pl time =
  let payload = Bytes.of_string pl in
  let (chest, chest_key_1) =
    Timelock.create_chest_and_chest_key ~payload ~time
  in
  let chest_key_2 = Timelock.create_chest_key ~time chest in
  let opening_result_1 = Timelock.open_chest chest chest_key_1 ~time in
  let opening_result_2 = Timelock.open_chest chest chest_key_2 ~time in
  let expected_opening_result = Timelock.(Correct payload) in

    let chest_key_bytes_1 =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn
                Timelock.chest_key_encoding
                chest_key_1))
    in
    let chest_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest))
    in
    print_endline (pretty_to_string (print_json_chest_key chest_bytes chest_key_bytes_1))

let force chest time =
  let chest_key_2 = Timelock.create_chest_key ~time chest in
  let chest_key_bytes_2 =
    "0x"
    ^ Hex.show
        (Hex.of_bytes
           (Data_encoding.Binary.to_bytes_exn
              Timelock.chest_key_encoding
              chest_key_2)) in
  print_endline chest_key_bytes_2

let main _ =
  Arg.parse speclist anon usage_msg;
  print_endline !value_arg;
  print_endline !chest_arg;
  print_endline (string_of_int !time_arg);
  if (compare !value_arg "") <> 0 then begin lock !value_arg !time_arg end(*;
  if (compare !chest_arg "") <> 0 then begin force !chest_arg !time_arg end *)
  (* TODO : check out to go from string to chest to force*)

let _ = main ()
