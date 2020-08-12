(**[try_rotate board tetromino cw] is Some new tetromino if the rotation
    (clockwise if [cw], countercw otherwise) is valid for [board]. None
    otherwise. *)
val try_rotate : Board.t -> Tetromino.t -> bool -> Tetromino.t option

(**[try_move board tetromino right] is Some new tetromino if the movement
    (a space right if [right], a space left otherwise) is valid for [board]. 
    None otherwise. *)
val try_move : Board.t -> Tetromino.t -> Tetromino.pos -> Tetromino.t option
