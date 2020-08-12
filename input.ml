open Yojson.Basic.Util

let keys_filepath = "config/keys.json"

type key_input = 
  | KMoveRight
  | KMoveLeft
  | KHold
  | KRotateRight
  | KRotateLeft
  | KHardDrop
  | KQuit
  | KRestart

type input = DoNothing
           | Rotate of bool
           | HardDrop
           | Hold
           | Move of bool

module CharMap = Map.Make(String)
module CommandMap = Map.Make(Char)

type t =
{
  mutable has_quit : bool;
  key_map : char CharMap.t;
  command_map : (t -> Game_state.t -> Game_state.t) CommandMap.t;
}

let key_input_to_string = function
  | KMoveRight -> "move_right"
  | KMoveLeft -> "move_left"
  | KRotateRight -> "rotate_right"
  | KRotateLeft -> "rotate_left"
  | KHold -> "hold"
  | KHardDrop -> "hard_drop"
  | KQuit -> "quit"
  | KRestart -> "restart"

let add_key_binding key_input config command (key_map, command_map) =
  let name = key_input_to_string key_input in
  let key_str = config |> member name |> to_string in 
  let key_ch = String.get key_str 0 in
  (CharMap.add name key_ch key_map, CommandMap.add key_ch command command_map) 

let add_key_binding_default key_input key_ch command (key_map, command_map) =
  CharMap.add 
     (key_input_to_string key_input) 
     key_ch key_map, 
     CommandMap.add key_ch command command_map 

let process_input_command command input_state st = 
  if Game_state.game_over st then st else begin
    match command with
    | Rotate cw -> Game_state.rotate cw st
    | Move right -> Game_state.move right st
    | HardDrop -> Game_state.hard_drop st
    | Hold -> Game_state.hold st
    | DoNothing -> st
  end

let init () =
  let config = Yojson.Basic.from_file keys_filepath in
  let (key_map, command_map) = 
    (CharMap.empty, CommandMap.empty)
    |> add_key_binding KMoveLeft config (process_input_command (Move false))
    |> add_key_binding KMoveRight config (process_input_command (Move true))
    |> add_key_binding KRotateLeft config (process_input_command (Rotate false))
    |> add_key_binding KRotateRight config (process_input_command (Rotate true))
    |> add_key_binding KHardDrop config (process_input_command HardDrop)
    |> add_key_binding KHold config (process_input_command Hold)
    |> add_key_binding_default KQuit 'q' 
      (fun input_state st -> input_state.has_quit <- true; st)
    |> add_key_binding_default KRestart 'r'
      (fun input_state st -> Game_state.init ())
  in 
{
  has_quit = false;
  key_map = key_map;
  command_map = command_map;
}

let has_quit input = input.has_quit

let get_key key_input input_st =
  CharMap.find (key_input_to_string key_input) input_st.key_map

let process_input_char input_state st ch =
  let maybe_command = CommandMap.find_opt ch input_state.command_map in
  match maybe_command with
  | Some command -> command input_state st
  | None -> process_input_command DoNothing input st  

let process_input input_state inputs st =
  let chars = List.init (String.length inputs) (String.get inputs) in
  List.fold_left (fun st c -> process_input_char input_state st c) st chars
