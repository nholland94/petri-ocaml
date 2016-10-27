open Batteries

let (>>) f g x = g (f x)

let list_sub base to_remove =
  let rec loop ls = function
      h :: t -> loop (if List.mem h ls then List.remove ls h else ls) t
    | []     -> ls
  in
  loop base to_remove

let array_collect_indices fn arr =
  let rec loop ls i =
    if i >= 0 then
      let ls = if fn arr.(i) then i :: ls else ls in
      loop ls (i - 1)
    else ls
  in
  loop [] (Array.length arr - 1)

let mat_map fn mat = Array.map (Array.map fn) mat
let mat_reduce fn mat = Array.reduce fn (Array.map (Array.reduce fn) mat)

let flatten_array arr =
  if Array.length arr = 0 then [||] else
    let rows = Array.length arr in
    let cols = Array.length arr.(0) in
    let flat_length = rows * cols in
    let flat_arr = Array.make flat_length arr.(0).(0) in
    let rec loop i j =
      if i < rows then
        if j < cols then begin
          flat_arr.(i * rows + j) <- arr.(i).(j);
          loop i (j + 1)
        end else loop (i + 1) 0
      else flat_arr
    in
    loop 0 0

let string_of_int_mat mat =
  let strings = mat_map string_of_int mat in
  let string_lengths = mat_map String.length strings in
  let max_string_length = mat_reduce max string_lengths in
  let pad_string str = str ^ String.make (max_string_length - String.length str) ' ' in
  let padded_strings = mat_map pad_string strings in
  let array_concat join arr = String.concat join (Array.to_list arr) in
  array_concat "\n" (Array.map (array_concat " ") padded_strings)
