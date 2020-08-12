module GravMap = Map.Make(Int)

let board_height = 20
let board_width = 10

type t = 
{
  board : Board.t;
  active_tetromino : Tetromino.t option;
  hold_tetromino : Tetromino.t option;
  piece_drop_counter : float;
  lock_delay_counter : float;
  lock_delay_max : float;
  can_hold : bool;
  piece_queue : Piece_queue.t;
  score : int;
  game_over : bool;
  lines_cleared : int;
  lines_to_win : int;
  level_thresholds : int list;
  grav_levels : float GravMap.t; (* gravity is lines per second *)
  animation : Animation.t;
}

let spawn_piece board tetromino = 
  let spawn_t = 
    tetromino |> 
    Tetromino.change_pos (
      (Board.get_width board) / 2, 
      (Board.get_height board) - 3
    ) 
  in
  if Board.is_valid_tetromino board spawn_t then Some spawn_t
  else None

let check_grav_level_compatibility st =
  let level_size = List.length st.level_thresholds in 
  let grav_size = GravMap.cardinal st.grav_levels in
  if level_size <> grav_size - 1
  then failwith "Level thresholds is not compatible with the gravity map!" 
  else st

let init () = 
  let piece_queue = Piece_queue.init 5 in
  let starting_piece = Piece_queue.next_piece piece_queue in
  let board = Board.init board_width board_height in
{
  board = board;
  active_tetromino = begin
    match spawn_piece board starting_piece with
    | None -> failwith "Spawn shouldn't be invalid at start of game!"
    | Some _ as sp -> sp
  end;
  hold_tetromino = None; 
  piece_drop_counter = 0.;
  lock_delay_counter = 0.;
  lock_delay_max = 0.5;
  can_hold = true;
  piece_queue = piece_queue;
  score = 0;
  game_over = false;
  lines_cleared = 0;
  lines_to_win = 150;
  level_thresholds = [15;30;45;60;75;90;105;120;135];
  grav_levels = 
    GravMap.empty 
    |> GravMap.add 1 (60. /. 40.)
    |> GravMap.add 2 (60. /. 30.)
    |> GravMap.add 3 (60. /. 20.)
    |> GravMap.add 4 (60. /. 15.)
    |> GravMap.add 5 (60. /. 10.)
    |> GravMap.add 6 (60. /. 8.)
    |> GravMap.add 7 (60. /. 6.)
    |> GravMap.add 8 (60. /. 4.)
    |> GravMap.add 9 (60. /. 2.)
    |> GravMap.add 10 (60. /. 1.);
  animation = Animation.init ();
} |> check_grav_level_compatibility

let board st = st.board
let active_tetromino st = st.active_tetromino
let hold_tetromino st = st.hold_tetromino
let piece_queue st = st.piece_queue
let score st = st.score
let game_over st = st.game_over || st.lines_cleared >= st.lines_to_win
let lines_cleared st = min st.lines_cleared st.lines_to_win
let lines_to_win st = st.lines_to_win
let animation st = st.animation

let get_game_status st = 
  if not (game_over st) then ""
  else if st.lines_cleared >= st.lines_to_win then "You won!"
  else "You lost :("

let level st =
  let lines_cleared = st.lines_cleared in
  List.fold_left (
    fun level threshold -> if lines_cleared >= threshold then level+1 else level
  ) 1 st.level_thresholds

let gravity st =
  let curr_level = level st in
  GravMap.find curr_level st.grav_levels

let try_lose_game st =
  { st with game_over = st.active_tetromino = None }

let spawn_new_piece st =
  let new_piece = 
    if game_over st then None 
    else spawn_piece st.board (Piece_queue.next_piece st.piece_queue)
  in
  { st with active_tetromino = new_piece }
  |> try_lose_game


let reset_lock_delay st =
  { st with lock_delay_counter = 0. }

let change_active_tetromino tetromino st = 
  { st with active_tetromino = Some tetromino }

let add_to_lock_delay amount st =
  { st with lock_delay_counter = st.lock_delay_counter +. amount }

let add_to_piece_drop amount st =
  { st with piece_drop_counter = st.piece_drop_counter +. amount }

let increment_score amount st = 
  { st with score = st.score + amount }

let increment_lines_cleared amount st = 
  { st with lines_cleared = st.lines_cleared + amount }

let reset_can_hold st = { st with can_hold = true }

let hold st =
  match st.active_tetromino with
  | None -> st
  | Some active_tetromino -> begin
    if st.can_hold then begin
      match st.hold_tetromino with
      | None -> begin
          { st with
            can_hold = false;
            hold_tetromino = Some (
              active_tetromino 
              |> Tetromino.change_orientation Tetromino.Zero);
          } |> spawn_new_piece
        end
      | Some hold_tetromino -> begin
        { st with 
          can_hold = false; 
          hold_tetromino = Some (
              active_tetromino 
              |> Tetromino.change_orientation Tetromino.Zero); 
          active_tetromino =  
              spawn_piece st.board hold_tetromino
        } |> try_lose_game
      end
    end
    else st
end

let rotate cw st =
  match st.active_tetromino with
  | None -> st
  | Some tetromino -> 
    match SRS.try_rotate st.board tetromino cw with
    | None -> st
    | Some tetromino -> 
      st
      |> change_active_tetromino tetromino
      |> reset_lock_delay

let move right st =
  match st.active_tetromino with
  | None -> st
  | Some tetromino ->
    let delta_pos = if right then (1,0) else (-1,0) in
    match SRS.try_move st.board tetromino delta_pos with
    | None -> st
    | Some tetromino -> 
      st
      |> change_active_tetromino tetromino
      |> reset_lock_delay

let calculate_score st lines_cleared = 
  let level = level st in
  match lines_cleared with
  | 1 -> 100 * level
  | 2 -> 300 * level
  | 3 -> 500 * level
  | 4 -> 800 * level
  | _ -> 0

let try_update_score (maybe_line_clear : Board.line_clear option) st =
  match maybe_line_clear with
  | None -> st
  | Some line_clear -> 
    let num_lines_cleared = List.length line_clear.lines in
    st 
    |> increment_score (calculate_score st num_lines_cleared) 
    |> increment_lines_cleared num_lines_cleared

let try_line_clear_animation (maybe_line_clear : Board.line_clear option) st =
  match maybe_line_clear with
  | None -> st
  | Some line_clear ->
    { st with 
      animation = Animation.do_line_clear_animation st.animation line_clear 0.25
    }

let drop_piece_one_line use_lock_delay st =
  match st.active_tetromino with
  | None -> (st, false)
  | Some tetromino ->
    match SRS.try_move st.board tetromino (0, -1) with
    | None -> 
      begin
        let st' = add_to_lock_delay (1. /. (gravity st)) st in
        if use_lock_delay && st'.lock_delay_counter < st'.lock_delay_max then
          (st', false)
        else 
          begin
            let maybe_line_clear = Board.place_tetromino st.board tetromino in
            (st' 
             |> try_update_score maybe_line_clear 
             |> try_line_clear_animation maybe_line_clear 
             |> spawn_new_piece 
             |> reset_lock_delay 
             |> reset_can_hold, true)
          end
      end
    | Some tetromino -> (change_active_tetromino tetromino st, false)

let rec hard_drop st =
  let (st', placed) = drop_piece_one_line false st in
  if placed then st'
  else hard_drop st'

let rec drop_lines use_lock_delay n st = 
  match n with
  | 0 -> st
  | _ -> 
    let st' = drop_piece_one_line use_lock_delay st |> fst in
    drop_lines use_lock_delay (n - 1) st'

let tick_animation animation dt = 
  let anim' = Animation.tick_elapsed animation dt in
  if Animation.animation_over anim' then Animation.init () 
  else anim' 

let update_animation dt st =
  let animation = tick_animation st.animation dt in
  { st with animation }

let update delta_time st =
  (if game_over st then st else begin
    let st' = st |> add_to_piece_drop delta_time in
    let grav = gravity st' in
    let lines_to_drop = (grav *. st'.piece_drop_counter) |> int_of_float in
    st' 
    |> drop_lines true lines_to_drop
    |> add_to_piece_drop ((float_of_int (-lines_to_drop)) /. grav)
  end)
  |> update_animation delta_time
