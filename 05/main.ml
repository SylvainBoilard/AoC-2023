let read_seeds channel =
  Scanf.sscanf (input_line channel) "seeds: %s@\n" (fun str ->
      Array.of_list (List.map int_of_string (String.split_on_char ' ' str)))

type mapping =
  { first : int
  ; last : int
  ; offset : int
  }

type step =
  | Map of map
  | Commit

let read_steps channel =
  let rec loop () = match input_line channel with
    | line ->
       begin
         try
           Scanf.sscanf line "%d %d %d" (fun target first length ->
               Map { first; last = first + length; offset = target - first } :: loop ())
         with
         | End_of_file -> loop ()
         | Scanf.Scan_failure _ -> Commit :: loop ()
       end
    | exception End_of_file -> []
  in
  loop ()

type range =
  { first : int
  ; last : int
  }

let rec insert_range range = function
  | hd :: tl as l when hd.last <= range.first ->
     let tl' = insert_range range tl in
     if tl' == tl then l else hd :: tl'
  | hd :: _ as l when range.last <= hd.first -> l
  | hd :: _ as l when hd.first <= range.first && range.last <= hd.last -> l
  | hd :: tl -> failwith "insert_range: unimplemented"
  | [] -> []

let part_1 seeds steps =
  let current = Array.copy seeds in
  let mapped = Array.copy current in
  List.iter (function
      | Map { first; last; offset } ->
         Array.iteri (fun i n ->
             if first <= n && n < last then
               mapped.(i) <- n + offset
           ) current
      | Commit -> Array.(blit mapped 0 current 0 (length current))
    ) steps;
  Array.fold_left min max_int mapped

let () =
  let seeds = read_seeds stdin in
  let steps = read_steps stdin in
  Printf.printf "%d\n%!" (part_1 seeds steps)
