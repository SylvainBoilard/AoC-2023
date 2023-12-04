let read_input () =
  let delim = Str.regexp " +" in
  let[@tail_mod_cons] rec loop () = match input_line stdin with
    | line ->
       let winning, numbers =
         Scanf.sscanf line "Card %_d: %s@| %s@\n" (fun winning numbers ->
             Str.split delim winning, Str.split delim numbers)
       in
       (winning, numbers) :: loop ()
    | exception End_of_file -> []
  in
  Array.of_list (loop ())

let () =
  let cards = read_input () in
  let matches =
    Array.init (Array.length cards) (fun i ->
        let winning, numbers = cards.(i) in
        List.fold_left (fun acc num -> if List.mem num winning then acc + 1 else acc) 0 numbers
      )
  in
  let score = ref 0 in
  for i = 0 to Array.length cards - 1 do
    score := !score + (1 lsl matches.(i)) lsr 1
  done;
  Printf.printf "%d\n%!" !score;
  let counts = Array.(make (length cards) 1) in
  for i = 0 to Array.length cards - 1 do
    for j = i + 1 to i + matches.(i) do
      counts.(j) <- counts.(j) + counts.(i)
    done
  done;
  Printf.printf "%d\n%!" (Array.fold_left (+) 0 counts)
