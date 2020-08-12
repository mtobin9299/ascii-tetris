type t = {
  queue : Tetromino.t Queue.t;
  piece_gen : Piece_gen.t;
  depth : int;
}

let enqueue_tetromino q piece_gen =
  let piece_type = Piece_gen.get_next_piece piece_gen in 
  let tetromino = Tetromino.init_tetromino (0,0) piece_type in
  Queue.add tetromino q

let init depth = 
  let piece_gen = Piece_gen.init () in
  let queue = Queue.create () in
  for i = 1 to depth do
    enqueue_tetromino queue piece_gen;
  done;
  {
    queue = queue;
    piece_gen = piece_gen;
    depth = depth;
  }

let next_piece q = 
  let piece = Queue.pop q.queue in
  enqueue_tetromino q.queue q.piece_gen;
  piece

let to_list q =
  Queue.fold (fun pieces piece -> piece :: pieces) [] q.queue |> List.rev
