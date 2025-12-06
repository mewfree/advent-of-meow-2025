let parse str =
  Scanf.sscanf str "%c%d" (fun dir num ->
    match dir with
    | 'L' -> -num
    | 'R' -> num
    | _ -> failwith "Invalid direction"
  )

let input =
  In_channel.with_open_text "example.txt" In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "")
  |> List.map parse

let scan_mod start values =
  let positive_mod n = let r = n mod 100 in if r < 0 then r + 100 else r in
  List.to_seq values
  |> Seq.scan (fun pos delta -> positive_mod (pos + delta)) start
  |> Seq.drop 1
  |> List.of_seq

let result = scan_mod 50 input

let zero_count =
  result
  |> List.filter (fun x -> x = 0)
  |> List.length

let () = print_endline ("Result: " ^ string_of_int zero_count)
