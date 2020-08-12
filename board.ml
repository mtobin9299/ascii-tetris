type pos = (int * int)
type mino = Tetromino.piece_type

type t = 
{
  board : mino option array array;
  width : int;
  height : int;
  visible_height : int;
}

type line_clear = 
{
  lines : int list;
}

let init w h = 
  let ext_h = h + 3 in
{
  board = Array.make_matrix ext_h w None;
  width = w;
  height = ext_h;
  visible_height = h;
}

let get_width board = board.width
let get_height board = board.height
let get_visible_height board = board.visible_height

let is_mino = function
  | None -> false
  | Some _ -> true

let has_mino board (x, y) = is_mino board.board.(y).(x)

let in_bounds board (x, y) =
  x >= 0 && x < board.width && y >= 0 && y < board.height

let is_valid board pos =
  (in_bounds board pos) && not (has_mino board pos)  

let is_valid_tetromino board tetromino =
  let minos = Tetromino.minos_world_positions tetromino in
  List.for_all (is_valid board) minos

let get_mino_at board (x, y) = board.board.(y).(x)

let place_mino board mino (x,y) =
  board.board.(y).(x) <- Some mino

let set_top_row_to_empty board =
  board.board.(board.height - 1) <- Array.make board.width None 

let rec clear_lines board = function
  | [] -> ()
  | line :: lines -> 
    let len = board.height - line - 1 in
    begin
      if len <> 0 then
        Array.blit board.board (line + 1) board.board line len
      else ()
    end;
    set_top_row_to_empty board;
    clear_lines board lines

let try_line_clear board =
  let lines_to_clear = ref [] in
  for i = 0 to board.height - 1 do
    let row = board.board.(i) in
    if Array.for_all is_mino row then
      lines_to_clear := i :: !lines_to_clear
  done;
  clear_lines board !lines_to_clear;
  if List.length !lines_to_clear <> 0 then
    Some { lines = !lines_to_clear }
  else None

let place_tetromino board tetromino =
  let minos_positions = Tetromino.minos_world_positions tetromino in
  List.iter (place_mino board (Tetromino.piece_type tetromino)) minos_positions;
  try_line_clear board 
