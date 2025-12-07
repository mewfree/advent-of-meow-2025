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

let () = print_endline ("Part 1: " ^ string_of_int zero_count)

let count_zeros start delta =
  let positive_mod n = let r = n mod 100 in if r < 0 then r + 100 else r in
  let end_pos = positive_mod (start + delta) in

  let count = match delta with
    | 0 -> 0
    | d when d > 0 ->
        let first_zero_at = if start = 0 then 100 else 100 - start in
        max 0 ((d - first_zero_at + 100) / 100)
    | d ->
        let first_zero_at = if start = 0 then 100 else start in
        max 0 ((-d - first_zero_at + 100) / 100)
  in
  (end_pos, count)

let crossings =
  let _, results = List.fold_left (fun (pos, acc) delta ->
    let new_pos, zeros = count_zeros pos delta in
    (new_pos, (pos, delta, new_pos, zeros) :: acc)
  ) (50, []) input in
  List.rev results

let part2 =
  crossings
  |> List.map (fun (_, _, _, zeros) -> zeros)
  |> List.fold_left (+) 0

let () = print_endline ("Part 2: " ^ string_of_int part2)
