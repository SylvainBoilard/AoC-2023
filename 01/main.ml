let int_of_digit_or_word = function
  | "0" | "zero" -> 0
  | "1" | "one" -> 1
  | "2" | "two" -> 2
  | "3" | "three" -> 3
  | "4" | "four" -> 4
  | "5" | "five" -> 5
  | "6" | "six" -> 6
  | "7" | "seven" -> 7
  | "8" | "eight" -> 8
  | "9" | "nine" -> 9
  | _ -> failwith "int_of_digit_or_word"

let () =
  let regexp = Str.regexp {|[0-9]\|zero\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|} in
  let rec loop acc = match input_line stdin with
    | str ->
       Str.search_forward regexp str 0 |> ignore;
       let n1 = int_of_digit_or_word (Str.matched_string str) in
       Str.search_backward regexp str (String.length str - 1) |> ignore;
       let n2 = int_of_digit_or_word (Str.matched_string str) in
       loop (acc + n1 * 10 + n2)
    | exception End_of_file -> acc
  in
  Printf.printf "%d\n%!" (loop 0)
