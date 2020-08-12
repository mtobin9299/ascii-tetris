(**[orientation] is the type representing the four standard orientations
   a tetromino can assume*)
type orientation = Zero | Right | Two | Left

(**[piece_type] is the type representing the seven different pieces there
   are in tetris*)
type piece_type = I | J | L | T | S | Z | O

(**[pos] is the type that represents an x, y coord pair *)
type pos = int * int

type t

(**[init_tetromino pos piece_type] is a new tetromino with position [pos] of
    type [piece_type]*)
val init_tetromino : pos -> piece_type -> t

(**[piece_type tetromino] is the piece type of [tetromino]*)
val piece_type : t -> piece_type

(**[orientation tetromino] is the orientation of [tetromino]*)
val orientation : t -> orientation

(**[offsets tetromino] is the standard list of all rotation offsets for each 
   possible orientation of [tetromino]*)
val offsets : t -> (orientation * pos list) list

(**[initial_positions tetromino] is the list of the relative, initial positions 
   of the minos that make up [tetromino]*)
val initial_positions : t -> pos list 

(**[pos tetromino] is the world position of [tetromino]*)
val pos : t -> pos

(**[minos_world_positions tetromino] is the list of world positions of the minos
    that make up [tetromino]*)
val minos_world_positions : t -> pos list

(**[rotate_minos orientation minos] is the list of minos with relative positions
    of [minos] in the orientation of [orientation]*)
val rotate_minos : orientation -> pos list -> pos list

(**[change_orientation orientation tetromino] is [tetromino] with new orientation
   [orientation] *)
val change_orientation : orientation -> t -> t

(**[change_pos pos tetromino] is [tetromino] with new position
   [pos] *)
val change_pos : pos -> t -> t

(**[move delta_pos tetromino] is [tetromino] with [delta_pos] added to its pos*)
val move : pos -> t -> t
