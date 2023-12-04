type element =
  | Number of int * int * int
  | Symbol of int * char

let read_input () =
  let rec aux_sym line acc i =
    if i >= String.length line then
      acc
    else
      match line.[i] with
      | '0'..'9' -> aux_num line acc i 0 0
      | '.' -> aux_sym line acc (i + 1)
      | c -> aux_sym line (Symbol (i, c) :: acc) (i + 1)
  and aux_num line acc i l n =
    if i >= String.length line then
      Number (i - l, l, n) :: acc
    else
      match line.[i] with
      | '0'..'9' as c -> aux_num line acc (i + 1) (l + 1) (n * 10 + Char.(code c - code '0'))
      | _ -> aux_sym line (Number (i - l, l, n) :: acc) i
  in
  let rec loop acc = match input_line stdin with
    | line -> loop (aux_sym line [] 0 :: acc)
    | exception End_of_file -> Array.of_list acc
  in
  loop []

let contains_symbol start finish elements =
  List.exists (function
      | Symbol (pos, _) when start - 1 <= pos && pos < finish + 1 -> true
      | _ -> false
    ) elements

let part_1 schematic =
  let line_count = Array.length schematic in
  let rec aux acc line = function
    | Symbol _ :: tl -> aux acc line tl
    | Number (start, length, number) :: tl ->
       let finish = start + length in
       if line > 0 && contains_symbol start finish schematic.(line - 1) 
          || contains_symbol start finish schematic.(line)
          || line + 1 < line_count && contains_symbol start finish schematic.(line + 1)
       then aux (acc + number) line tl
       else aux acc line tl
    | [] when line + 1 < line_count -> aux acc (line + 1) schematic.(line + 1)
    | [] -> acc
  in
  aux 0 0 schematic.(0)

let filter_numbers position elements =
  List.filter (function
      | Number (start, length, _) when position >= start - 1 && start + length + 1 > position -> true
      | _ -> false
    ) elements

let part_2 schematic =
  let line_count = Array.length schematic in
  let rec aux acc line = function
    | Symbol (position, '*') :: tl ->
       let prev = if line - 1 < 0 then [] else filter_numbers position schematic.(line - 1) in
       let curr = filter_numbers position schematic.(line) in
       let next = if line + 1 >= line_count then [] else filter_numbers position schematic.(line + 1) in
       begin match prev @ curr @ next with
       | Number (_, _, n1) :: Number (_, _, n2) :: [] -> aux (acc + n1 * n2) line tl
       | _ -> aux acc line tl
       end
    | _ :: tl -> aux acc line tl
    | [] when line + 1 < line_count -> aux acc (line + 1) schematic.(line + 1)
    | [] -> acc
  in
  aux 0 0 schematic.(0)

let print_schematic schematic =
  Array.iteri (fun line elements ->
      Printf.eprintf "%3d:" line;
      let prefix = ref "" in
      List.iter (fun elem ->
          begin match elem with
          | Number (i, l, n) -> Printf.eprintf "%sNumber %d at %d, length = %d" !prefix n i l
          | Symbol (i, c) -> Printf.eprintf "%sSymbol %c at %d" !prefix c i
          end;
          prefix := "; "
        ) elements;
      Printf.eprintf "\n"
    ) schematic

let () =
  let schematic = read_input () in
  Printf.printf "%d\n%d\n%!" (part_1 schematic) (part_2 schematic)
