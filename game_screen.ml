open ANSITerminal
open Tetromino

type pixel_type = 
  | CharPixel of char
  | UnicodePixel of string

type pixel = pixel_type * style list

type pos = int * int

type t =
{
  width : int;
  height : int;
  pixels : pixel array array;

  mutable elapsed : float;
  mutable frames : int;
  mutable frames_per_second : int;
}

let clear_pixel = CharPixel (' '), [white]

let block_char = UnicodePixel (Printf.sprintf "\xe2\x96\x93")
let ghost_char = UnicodePixel (Printf.sprintf "\xe2\x96\x91")
let first = UnicodePixel (Printf.sprintf "\xe2\x96\x93")
let second = UnicodePixel (Printf.sprintf "\xe2\x96\x92")
let third = UnicodePixel (Printf.sprintf "\xe2\x96\x91")

let border_bottom_char = CharPixel '-'
let border_side_char = CharPixel '|'

let board_pos = (17,5)
let hold_box_dim = (12,6)
let hold_box_xoff = 2 
let piece_queue_box_xoff = 2 
let piece_queue_depth = 5
let piece_height = 4
let piece_xoff = 4

let get_mino_color = function
  | I -> cyan
  | T -> magenta
  | Z -> red
  | S -> green
  | J -> blue
  | L -> white
  | O -> yellow

let init_screen tw th =
{
  width = tw;
  height = th;
  pixels = Array.make_matrix th tw clear_pixel;

  elapsed = 0.;
  frames = 0;
  frames_per_second = 0;
}

let get_board_dimensions board =
  (Board.get_width board, Board.get_visible_height board)

let get_hold_box_pos board =
  let (_,bh) = get_board_dimensions board in
  let (w,h) = hold_box_dim in
  let (bx,by) = board_pos in
  (bx - w - hold_box_xoff, by + bh - h + 2)

let get_piece_queue_dim =
  let w = 12 in
  let h = piece_queue_depth * piece_height + 2 in
  (w, h)

let get_piece_queue_pos board =
  let (bw,bh) = get_board_dimensions board in
  let (w,h) = get_piece_queue_dim in
  let (bx,by) = board_pos in
  (bx + 2*bw + piece_queue_box_xoff, by + bh - h + 2)

let clear_screen screen =
  for i = 0 to (screen.height - 1) do
    for j = 0 to (screen.width - 1) do
      screen.pixels.(i).(j) <- clear_pixel
    done
  done

let draw_pixel screen (x,y) pixel =
  screen.pixels.(y).(x) <- pixel

let draw_mino ?(two_wide=true) ?(block_char=block_char) screen maybe_mino x y =
  let pixel = begin 
    match maybe_mino with
    | Some mino -> (block_char, [get_mino_color mino])
    | None -> clear_pixel
  end in
  screen.pixels.(y).(x) <- pixel;
  if two_wide then screen.pixels.(y).(x + 1) <- pixel else ()

let draw_tetromino screen tetromino =
  let (start_x, _) = Tetromino.pos tetromino in
  let mino_positions = Tetromino.minos_world_positions tetromino in
  List.iter (fun (x,y) -> 
        draw_mino screen (Some (Tetromino.piece_type tetromino))
          (2*(x - start_x)+start_x) y ~two_wide:true) mino_positions 

let print_board_tetromino screen board block_char tetromino board_x board_y =
  let mino_positions = Tetromino.minos_world_positions tetromino in
  List.iter (fun (x,y) -> 
      if y < Board.get_visible_height board then
        draw_mino screen (Some (Tetromino.piece_type tetromino))
          (2*x+board_x) (y+board_y) ~two_wide:true ~block_char:block_char)
        mino_positions 

let print_active_tetromino screen board tetromino board_x board_y =
  print_board_tetromino screen board block_char tetromino board_x board_y

let rec ghost_tetromino board last_valid =
  let down_one = Tetromino.move (0, -1) last_valid in
  if Board.is_valid_tetromino board down_one then ghost_tetromino board down_one
  else last_valid

let get_ghost_tetromino board tetromino =
  ghost_tetromino board tetromino

let print_ghost_tetromino screen board tetromino board_x board_y =
  let ghost_piece = get_ghost_tetromino board tetromino in 
  print_board_tetromino screen board ghost_char ghost_piece board_x board_y

let print_board screen board board_x board_y =
  let height = Board.get_visible_height board in
  let width = Board.get_width board in
  for i = 0 to (height - 1) do
    for j = 0 to (width - 1) do
      draw_mino screen (Board.get_mino_at board (j, i)) (2*j+board_x)
        (i+board_y) ~two_wide:true
    done
  done

let print_rect_outline
    ?(render_top=true) ?(style=white) screen x y w h
    horizontal_pixel_type vertical_pixel_type =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let x = j + x in
      let y = i + y in
      if i = 0 || (i = (h-1) && render_top) then
        screen.pixels.(y).(x) <- (horizontal_pixel_type, [style])
      else if j = 0 || j = (w-1) then
        screen.pixels.(y).(x) <- (vertical_pixel_type, [style])
      done
  done

let print_board_border screen board board_x board_y =
  let height = Board.get_visible_height board in
  let width = Board.get_width board in
  print_rect_outline screen (board_x-1) (board_y-1) (2*width+2) (height+2)
    border_bottom_char border_side_char ~render_top:false

let print_hold_box screen st x y =
  let (w,h) = hold_box_dim in
  print_rect_outline screen x y w h border_bottom_char border_side_char;
  match Game_state.hold_tetromino st with
  | None -> ()
  | Some tetromino -> begin 
      let positioned_tetromino = 
        Tetromino.change_pos (x+piece_xoff,y+2) tetromino in
      draw_tetromino screen positioned_tetromino
    end

let print_piece_queue screen st x y =
  let (w,h) = get_piece_queue_dim in
  print_rect_outline screen x y w h border_bottom_char border_side_char;
  let piece_queue = Game_state.piece_queue st in
  let pq_list = Piece_queue.to_list piece_queue in
    List.iteri (fun i tetromino -> 
      tetromino 
      |> Tetromino.change_pos (x + piece_xoff, y + h - (i + 1) * piece_height) 
      |> draw_tetromino screen
    ) pq_list

let draw_screen screen =
  let (_, term_height) = ANSITerminal.size () in
  set_cursor 0 (term_height - screen.height);
  for i = 0 to (screen.height - 1) do 
    for j = 0 to (screen.width - 1) do
      let (pixel, styles) = screen.pixels.(screen.height - i - 1).(j) in
      match pixel with
      | CharPixel c -> print_string styles (Char.escaped c)
      | UnicodePixel u -> print_string styles u
    done;
    print_string [white] "\n";
  done

let rec draw_chars screen x y = function
  | [] -> ()
  | ch :: chars -> 
    screen.pixels.(y).(x) <- (CharPixel ch, [white]);
    draw_chars screen (x + 1) y chars 

let rec draw_string screen x y str = 
  draw_chars screen x y (List.init (String.length str) (String.get str))

let upper_key key_input input =
  Input.get_key key_input input |> Char.uppercase_ascii

let draw_game_info screen st x y = 
  draw_string screen x y ("Score: " ^ (Game_state.score st |> string_of_int)); 
  let lines_cleared = Game_state.lines_cleared st in
  let lines_to_win = Game_state.lines_to_win st in
  draw_string screen x (y-1) 
      (Printf.sprintf "Lines: %i/%i" lines_cleared lines_to_win); 
  draw_string screen x (y-2) ("Level: " ^ (Game_state.level st |> string_of_int))

let print_controls screen input x y =
  let rotate_left  = upper_key Input.KRotateLeft input in
  let rotate_right = upper_key Input.KRotateRight input in
  let move_left    = upper_key Input.KMoveLeft input in
  let move_right   = upper_key Input.KMoveRight input in
  let hold         = upper_key Input.KHold input in
  let hard_drop    = upper_key Input.KHardDrop input in
  let quit         = upper_key Input.KQuit input in
  let restart      = upper_key Input.KRestart input in
  draw_string screen x y ("Controls:"); 
  draw_string screen x (y-1) ("---------"); 
  draw_string screen x (y-2) 
    (Printf.sprintf "Rotate: %c/%c" rotate_left rotate_right); 
  draw_string screen x (y-3) (Printf.sprintf "Move: %c/%c" move_left move_right);
  draw_string screen x (y-4) (Printf.sprintf "Drop: %c" hard_drop); 
  draw_string screen x (y-5) (Printf.sprintf "Hold: %c" hold); 
  draw_string screen x (y-6) (Printf.sprintf "Quit: %c" quit);
  draw_string screen x (y-7) (Printf.sprintf "Restart: %c" restart)

let draw_line_clear screen st (animation:Animation.t) board_x board_y
    (line_clear : Board.line_clear) =
  let (bw,_) = get_board_dimensions (Game_state.board st) in
  let pct = Animation.pct_complete animation in
  let draw_line pixel line = 
    for x = 0 to (bw - 1) do
      let styles = [white] in 
      draw_pixel screen (2*x + board_x, line + board_y) (pixel, styles);
      draw_pixel screen (2*x+1 + board_x, line + board_y) (pixel, styles)
    done
  in
  let get_pixel pct_done =
    if pct_done < 0.33 then first
    else if pct_done < 0.67 then second
    else third
  in
  List.iter (draw_line (get_pixel pct)) line_clear.lines

let draw_animation ?(board_x=0) ?(board_y=0) screen st = 
  let animation = Game_state.animation st in
  match Animation.active_animation animation with
  | Nothing -> ()
  | LineClear line_clear -> 
    draw_line_clear screen st animation board_x board_y line_clear 

let render_screen screen delta_time input st =
  screen.elapsed <- screen.elapsed +. delta_time;
  screen.frames <- screen.frames + 1;

  if screen.elapsed >= 1. then 
    begin
      screen.frames_per_second <- screen.frames;
      screen.frames <- 0;
      screen.elapsed <- 0.;
    end;

  clear_screen screen;
  draw_string screen 1 5 ("FPS: " ^ (string_of_int screen.frames_per_second)); 

  print_controls screen input 1 19;
  draw_game_info screen st 1 9;

  draw_string screen 1 10 (Game_state.get_game_status st);
  
  let board = Game_state.board st in
  let (board_x, board_y) = board_pos in
  print_board screen board board_x board_y;
  print_board_border screen board  board_x board_y;

  let (hold_x, hold_y) = get_hold_box_pos board in
  print_hold_box screen st hold_x hold_y;

  let (queue_x, queue_y) = get_piece_queue_pos board in
  print_piece_queue screen st queue_x queue_y;

  begin
    match Game_state.active_tetromino st with
    | None -> ()
    | Some tetromino -> 
      print_ghost_tetromino screen board tetromino board_x board_y;
      print_active_tetromino screen board tetromino board_x board_y;
  end;

  draw_animation screen st ~board_x ~board_y;
     
  draw_screen screen;
  flush stdout
