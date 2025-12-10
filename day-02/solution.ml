let input =
  In_channel.with_open_text "example.txt" In_channel.input_all
  |> String.trim
  |> String.split_on_char ','

let parse str =
  Scanf.sscanf str "%d-%d" (fun start_id end_id -> (start_id, end_id))

let ranges = List.map parse input

let is_invalid_id n =
  let s = string_of_int n in
  let len = String.length s in
  len mod 2 = 0 &&
  let half = len / 2 in
  String.sub s 0 half = String.sub s half half

let find_invalid_ids_in_range (start_id, end_id) =
  let rec aux acc current =
    if current > end_id then List.rev acc
    else aux (if is_invalid_id current then current :: acc else acc) (current + 1)
  in
  aux [] start_id

let all_invalid_ids =
    List.concat_map find_invalid_ids_in_range ranges

let sum = List.fold_left (+) 0 all_invalid_ids

let () = print_endline ("Result: " ^ string_of_int sum)
