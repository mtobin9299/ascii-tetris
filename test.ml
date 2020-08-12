open OUnit2 

(* Testing
 * =========
 *
 * OUnit:
 * ========
 * The following modules were tested automatically with OUnit: Animation, Board,
 * Utils, and Tetromino. The main testing method we used in the unit tests is
 * black-box testing. Starting from the specs we figured out what the main use
 * of each function was as well as what possible corner cases there could be and
 * then wrote tests for each case. While this doesn't ensure that every branch
 * of code is reached in the testing process, based on our implementation of the
 * game and how the functions are used, we believe our test cases cover all
 * necessary uses of the functions.
 *
 * Animation.ml:
 * ------------
 * Handles the state of an animation. Functions tested:
 * - active_animation
 * - elapsed
 * - duration
 * - pct_complete
 * - init
 * - reset_elapsed
 * - tick_elapsed
 * - animation_over
 * - reset_animation
 * - do_line_clear_animation
 *
 * Board.ml:
 * ---------
 * Handles operations regarding the tetris board. Functions tested:
 * - init
 * - get_width
 * - get_height
 * - get_visible_height
 * - has_mino
 * - in_bounds
 * - is_valid
 * - is_valid_tetromino
 * - get_mino_at
 * - place_tetromino
 *
 * Utils.ml:
 * ---------
 * Utils is a module that contains utility functions. Currently, Utils only has
 * the function add_pos.
 *
 * Tetromino.ml:
 * -------------
 * Handles the state of a game piece (orientation, position, type, etc).
 * Functions tested:
 * - init_tetromino 
 * - piece_type 
 * - orientation 
 * - initial_positions 
 * - pos 
 * - minos_world_positions 
 * - rotate_minos
 * - change_orientation
 * - change_pos
 * - move
 * Note: the function offsets was not tested because the only thing to test
 * about it is its initial state (since it never changes), and doing so would be
 * pointless because it's predefined in Tetromino.
 *
 * Manual Testing
 * ===============
 * All other modules such as Game_screen, Game_state, Main, Input, Piece_gen,
 * Piece_queue and SRS don't lend themselves to unit testing since they comprise
 * an integrated system. This system is connected in ways that would make unit
 * testing more of a hindrance than a benefit. 
 *
 * To ensure the correctness of the program we depended on extensive
 * playtesting. This involved playing our game after each new feature was
 * implemented, trying to find bugs in the code. While playtesting can't ensure
 * there are zero bugs in our implementation definitively, it does serve as
 * sufficient means of guaranteeing correctness in the vast majority of gameplay
 * scenarios. We've tested corner cases and played the game to completion multiple
 * times to ensure validity giving us confidence that we have a working,
 * bug-free implementation of Tetris.
 *)

let make_fun2_test name f x y expected = 
  name >:: (fun _ -> assert_equal expected (f x y))

let make_basic_test ?(printer=fun _ -> "") name result expected =
  name >:: (fun _ -> assert_equal expected result ~printer) 

let make_bool_test name result =
  name >:: (fun _ -> assert result)

let anim = Animation.init ()
let lc : Board.line_clear = { lines = [1;2;3] } 
let lc_anim = 
  Animation.do_line_clear_animation anim lc 0.25
let lc_anim_t = Animation.tick_elapsed lc_anim 0.1
let lc_anim_fin = Animation.tick_elapsed lc_anim 0.25
let lc_reset = Animation.reset_animation lc_anim_fin 

let animation_tests = [
  make_basic_test "Elapsed = 0" (Animation.elapsed anim) 0.;
  make_basic_test "Duration = 0" (Animation.duration anim) 0.;
  make_basic_test "Active Animation = Nothing" (Animation.active_animation anim)
  Animation.Nothing;
  make_basic_test "LC Anim Elapsed = 0" (Animation.elapsed lc_anim) 0.;
  make_basic_test "LC Anim Duration = 0.25" (Animation.duration lc_anim) 0.25;
  make_basic_test "LC Active Animation = LC" 
    (Animation.active_animation lc_anim) (LineClear lc);
  make_bool_test "LC Animation Over = false" 
    (not (Animation.animation_over lc_anim));
  make_basic_test "LCT Elapsed = 0.1" (Animation.elapsed lc_anim_t) 0.1; 
  make_basic_test "LCT Duration = 0.25" (Animation.duration lc_anim_t) 0.25; 
  make_basic_test "LCT Pct = 0.4" (Animation.pct_complete lc_anim_t) 0.4; 
  make_bool_test "LCT Animation Over = false" 
    (not (Animation.animation_over lc_anim_t));
  make_basic_test "LCF Elapsed = 0.25" (Animation.elapsed lc_anim_fin) 0.25; 
  make_basic_test "LCF Duration = 0.25" (Animation.duration lc_anim_fin) 0.25; 
  make_basic_test "LCF Pct = 1.0" (Animation.pct_complete lc_anim_fin) 1.0; 
  make_bool_test "LCF Animation Over = true" 
    (Animation.animation_over lc_anim_fin);
  make_basic_test "LCR Elapsed = 0" (Animation.elapsed lc_reset) 0.; 
  make_basic_test "LCR Duration = 0" (Animation.duration lc_reset) 0.; 
  make_basic_test "LCR Active Animation = Nothing" 
    (Animation.active_animation lc_reset) Animation.Nothing; 
]

let smallb = Board.init 1 1
let board3 = Board.init 3 1
let t_piece = Tetromino.init_tetromino (1,0) Tetromino.T  
let t_invalid = t_piece |> Tetromino.move (1,0)
let board3' = Board.init 3 1
let m_lc = Board.place_tetromino board3' t_piece 

let board_tests = [
  make_basic_test "SB width = 1" (Board.get_width smallb) 1;
  make_basic_test "SB height = 4" (Board.get_height smallb) 4;
  make_basic_test "SB visible height = 1" (Board.get_visible_height smallb) 1;
  make_bool_test "SB no mino" (not (Board.has_mino smallb (0,0)));
  make_bool_test "SB in bounds" (Board.in_bounds smallb (0,0));
  make_bool_test "SB OOB" (Board.in_bounds smallb (1,0) |> not);
  make_bool_test "T-Piece valid" (Board.is_valid_tetromino board3 t_piece);
  make_bool_test "T-Piece invalid" 
    (Board.is_valid_tetromino board3 t_invalid |> not);
  make_basic_test "T-Piece LC" m_lc (Some { lines = [0] }); 
  make_basic_test "B3' Mino Loc (Some)" 
    (Board.get_mino_at board3' (1,0)) (Some Tetromino.T);
  make_basic_test "B3' Mino Loc (None)" 
    (Board.get_mino_at board3' (2,0)) None;
]

let i_piece = Tetromino.init_tetromino (0,0) Tetromino.I
let i_minos = i_piece |> Tetromino.initial_positions
let ir_minos = i_minos |> Tetromino.rotate_minos Tetromino.Right 
let il_minos = i_minos |> Tetromino.rotate_minos Tetromino.Left 
let i2_minos = i_minos |> Tetromino.rotate_minos Tetromino.Two 
let iz_minos = i_minos |> Tetromino.rotate_minos Tetromino.Zero 
let i_minos_4r = 
  i_minos
  |> Tetromino.rotate_minos Tetromino.Right
  |> Tetromino.rotate_minos Tetromino.Right
  |> Tetromino.rotate_minos Tetromino.Right
  |> Tetromino.rotate_minos Tetromino.Right
let i_minos_4l = 
  i_minos
  |> Tetromino.rotate_minos Tetromino.Left
  |> Tetromino.rotate_minos Tetromino.Left
  |> Tetromino.rotate_minos Tetromino.Left
  |> Tetromino.rotate_minos Tetromino.Left
let i_minos_22 = 
  i_minos
  |> Tetromino.rotate_minos Tetromino.Two
  |> Tetromino.rotate_minos Tetromino.Two
let i_minos_world =  
  i_piece 
  |> Tetromino.move (1,1) 
  |> Tetromino.minos_world_positions

let pp_list_tuple lst =
  let str = 
    List.fold_left (fun acc (x,y) -> acc ^ Printf.sprintf "(%i,%i)" x y) "[" lst
  in str ^ "]"

let tetromino_tests = [
  make_basic_test "I Piece type = I" (Tetromino.piece_type i_piece) Tetromino.I;
  make_basic_test "I Orientation = Zero" 
    (Tetromino.orientation i_piece) Tetromino.Zero;
  make_basic_test "I Initial Positions" 
    (Tetromino.initial_positions i_piece) [(-1,0);(0,0);(1,0);(2,0)];

  (* Rotate Minos*)
  make_basic_test "I Right" ir_minos [(0,1);(0,0);(0,-1);(0,-2)] 
    ~printer:pp_list_tuple;
  make_basic_test "I Left" il_minos [(0,-1);(0,0);(0,1);(0,2)] 
    ~printer:pp_list_tuple;
  make_basic_test "I Two" i2_minos [(1,0);(0,0);(-1,0);(-2,0)] 
    ~printer:pp_list_tuple;
  make_basic_test "I Zero" iz_minos i_minos ~printer:pp_list_tuple;
  make_basic_test "I 4 Right (Identity)" iz_minos i_minos_4r 
    ~printer:pp_list_tuple;
  make_basic_test "I 4 Left (Identity)" iz_minos i_minos_4l 
    ~printer:pp_list_tuple;
  make_basic_test "I 2 Two (Identity)" iz_minos i_minos_22 
    ~printer:pp_list_tuple;

  (* Move *)
  make_basic_test "I Move 1" (
    i_piece |> Tetromino.move (1,1) |> Tetromino.pos) (1,1);
  make_basic_test "I Move 2" (
    i_piece |> Tetromino.move (-1,1) |> Tetromino.pos) (-1,1);
  make_basic_test "I Move Identity" (
    i_piece |> Tetromino.move (0,0) |> Tetromino.pos) (Tetromino.pos i_piece);

  (* Change Pos *)
  make_basic_test "I Change 1" (
    Tetromino.(i_piece |> change_pos (1,1) |> pos)) (1,1);
  make_basic_test "I Move 2" (
    Tetromino.(i_piece |> change_pos (-1,-1) |> pos)) (-1,-1);
  make_basic_test "I Move Identity" (
    Tetromino.(i_piece |> change_pos (0,0) |> pos)) (Tetromino.pos i_piece);

  (* World Pos *)
  make_basic_test "I World" i_minos_world [(0,1);(1,1);(2,1);(3,1)] 
    ~printer:pp_list_tuple; 

  (* Orientation *)
  make_basic_test "I Orientation Right"
    Tetromino.(i_piece |> change_orientation Right |> orientation)
    Tetromino.Right;
  make_basic_test "I Orientation Right"
    Tetromino.(i_piece |> change_orientation Zero |> orientation)
    Tetromino.Zero;
  make_basic_test "I Orientation Right"
    Tetromino.(i_piece |> change_orientation Left |> orientation)
    Tetromino.Left;
  make_basic_test "I Orientation Right"
    Tetromino.(i_piece |> change_orientation Two |> orientation)
    Tetromino.Two;
]

let util_tests = [
  make_fun2_test "add tuples zero" Utils.add_pos (0,0) (0,0) (0,0);
  make_fun2_test "add tuples identity" Utils.add_pos (0,0) (1,2) (1,2);
  make_fun2_test "add tuples negative" Utils.add_pos (1,2) (-3,-4) (-2,-2);
  make_fun2_test "add tuples normal" Utils.add_pos (1,2) (3,4) (4,6);
]

let tests = "Tetris Test Suite" >::: List.flatten [
    animation_tests;
    board_tests;
    tetromino_tests;
    util_tests;
  ]

let _ = run_test_tt_main tests
