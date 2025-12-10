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

let () = print_endline ("Part 1: " ^ string_of_int sum)

let is_invalid_id_p2 n =
  let s = string_of_int n in
  let len = String.length s in
  len > 1 &&
  let rec check_pattern pattern_len =
    if pattern_len > len / 2 then false
    else if len mod pattern_len = 0 then
      let pattern = String.sub s 0 pattern_len in
      let rec all_match pos =
        pos >= len ||
        (String.sub s pos pattern_len = pattern && all_match (pos + pattern_len))
      in
      all_match pattern_len || check_pattern (pattern_len + 1)
    else check_pattern (pattern_len + 1)
  in
  check_pattern 1

let find_invalid_ids_in_range_p2 (start_id, end_id) =
  let rec aux acc current =
    if current > end_id then List.rev acc
    else aux (if is_invalid_id_p2 current then current :: acc else acc) (current + 1)
  in
  aux [] start_id

let all_invalid_ids_p2 =
    List.concat_map find_invalid_ids_in_range_p2 ranges

let sum_p2 = List.fold_left (+) 0 all_invalid_ids_p2

let () = print_endline ("Part 2: " ^ string_of_int sum_p2)
