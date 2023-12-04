let delim = Str.regexp "[,;] "

let read_cubes str =
  Str.split_delim delim str
  |> List.map (fun record -> Scanf.sscanf record "%d %s" (fun count color -> count, color))

let part_1 () =
  let rec loop acc = match input_line stdin with
    | line ->
       let id, str = Scanf.sscanf line "Game %d: %s@\n" (fun id str -> id, str) in
       read_cubes str
       |> List.for_all (fun (count, color) ->
              match color with
              | "red" when count > 12 -> false
              | "green" when count > 13 -> false
              | "blue" when count > 14 -> false
              | _ -> true
            )
       |> (function true -> loop (acc + id) | false -> loop acc)
    | exception End_of_file -> acc
  in
  loop 0

let part_2 () =
  let rec loop acc = match input_line stdin with
    | line ->
       let _id, str = Scanf.sscanf line "Game %d: %s@\n" (fun id str -> id, str) in
       read_cubes str
       |> List.fold_left (fun (red, green, blue) (count, color) ->
              match color with
              | "red" -> max count red, green, blue
              | "green" -> red, max count green, blue
              | "blue" -> red, green, max count blue
              | _ -> assert false
            ) (0, 0, 0)
       |> fun (red, green, blue) -> loop (acc + red * green * blue)
    | exception End_of_file -> acc
  in
  loop 0

let () =
  Printf.printf "%d\n%!" (part_2 ())
