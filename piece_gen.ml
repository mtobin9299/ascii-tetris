open Tetromino

type t = {
  mutable bag : piece_type list 
}

let pieces = [I;J;L;S;Z;O;T]

let shuffle lst = 
  Random.init (Unix.time () |> int_of_float);
  let nd = List.map (fun c -> Random.bits (), c) lst in
  let sond = List.sort compare nd in
  List.map snd sond

let gen_bag () = shuffle pieces

let init () = {
  bag = gen_bag ()
}

let rec get_next_piece piece_gen =
  match piece_gen.bag with
  | [] -> piece_gen.bag <- gen_bag (); 
    get_next_piece piece_gen
  | piece :: pieces -> 
    piece_gen.bag <- pieces;
    piece
  
