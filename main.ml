open Lwt.Infix
open ANSITerminal

let sleep_time = 0.001

let current_input = ref ""

let last_time = ref 0.

let screen = 
  let (tw, th) = size () in
  Game_screen.init_screen tw (th - 1)

let input =
  Input.init ()

let setup_terminal () =
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW
    { termio with Unix.c_icanon = false };
  let _ = Sys.command "setterm -cursor off; stty -echo" in
  erase Screen

let restore_terminal () =
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW
    { termio with Unix.c_icanon = true };
  let _ = Sys.command "setterm -cursor on; stty echo" in
  erase Screen

let rec loop st =
  (* Calculate delta *)
  let current_time = Unix.gettimeofday () in
  let delta = current_time -. !last_time in
  last_time := current_time;

  let st' = 
    st 
    |> Input.process_input input !current_input 
    |> Game_state.update delta in
  current_input := "";

  if Input.has_quit input then begin
    restore_terminal ();
    exit 0
  end;

  Game_screen.render_screen screen delta input st';
  Lwt_unix.sleep sleep_time >>= fun () ->
  loop st'

let init_game =
  Game_state.init ()

let game_loop =
  last_time := Unix.time ();
  setup_terminal ();
  loop init_game 

let rec read_input () = 
  Lwt_io.(read_char stdin) >>= fun c ->
  current_input := !current_input ^ (Char.escaped c); 
  read_input () 

let _ = Lwt_main.run 
    (Lwt.join [read_input (); game_loop])

