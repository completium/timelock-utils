open Tezos_crypto

let main _ =
  let payload = Bytes.of_string "\x05\x00\x80\x89\x0f" and time = 3600 in
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
        let chest_key_bytes_2 =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn
                Timelock.chest_key_encoding
                chest_key_2))
    in
    let chest_bytes =
      "0x"
      ^ Hex.show
          (Hex.of_bytes
             (Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest))
    in

    print_endline "chest_key_bytes_1";
    print_endline chest_key_bytes_1;
    print_endline "chest_key_bytes_2";
    print_endline chest_key_bytes_2;
    print_endline "chest_bytes";
    print_endline chest_bytes

let _ = main ()
