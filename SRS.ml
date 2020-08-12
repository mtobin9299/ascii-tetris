open Tetromino
open Utils

let next_orientation_cw = function
  | Zero -> Right
  | Right -> Two
  | Two -> Left
  | Left -> Zero

let next_orientation_ccw = function
  | Right -> Zero
  | Two -> Right
  | Left -> Two
  | Zero -> Left

let get_offsets tetromino from_o to_o = 
  let from_offs = List.assoc from_o (offsets tetromino) in
  let to_offs = List.assoc to_o (offsets tetromino) in
  List.map2 (
    fun (from_x, from_y) (to_x, to_y) -> (from_x - to_x, from_y - to_y)
  ) from_offs to_offs

let check_valid board minos =
  List.for_all (Board.is_valid board) minos

let try_offset board rotated_minos offset pos =
  let board_minos = List.map (offset |> add_pos pos |> add_pos) rotated_minos in
  check_valid board board_minos

let rec try_offsets board tetromino rotated_minos = function
  | [] -> None
  | offset :: offsets -> 
    if try_offset board rotated_minos offset (pos tetromino)
    then Some offset 
    else try_offsets board tetromino rotated_minos offsets

let try_rotate board tetromino cw = 
  let from_o = orientation tetromino in
  let to_o = if cw 
    then next_orientation_cw from_o
    else next_orientation_ccw from_o in 
  let rotated_minos = rotate_minos to_o (initial_positions tetromino)
  in
  let offsets = get_offsets tetromino from_o to_o in
  match try_offsets board tetromino rotated_minos offsets with
  | None -> None
  | Some offset -> Some (
    tetromino 
    |> change_orientation to_o 
    |> move offset
  )

let try_move board tetromino delta_pos  =
  let new_tetromino = move delta_pos tetromino in
  let valid = check_valid board (minos_world_positions new_tetromino) in
  if valid then Some new_tetromino
  else None
