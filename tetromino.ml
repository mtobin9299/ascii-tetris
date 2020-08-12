type orientation = Zero | Right | Two | Left
type piece_type = I | J | L | T | S | Z | O
type pos = int * int

type t = 
{
  pos : int * int;
  orientation:  orientation;
  piece_type : piece_type;
  initial_minos_position : pos list;
  offsets : (orientation * pos list) list;
}

let piece_type tetromino = tetromino.piece_type
let orientation tetromino = tetromino.orientation
let offsets tetromino = tetromino.offsets
let pos tetromino = tetromino.pos
let initial_positions tetromino = tetromino.initial_minos_position

let standard_offsets = [
  (Zero, [(0,0);(0,0);(0,0);(0,0);(0,0)]);
  (Right, [(0,0);(1,0);(1,-1);(0,2);(1,2)]);
  (Two, [(0,0);(0,0);(0,0);(0,0);(0,0)]);
  (Left, [(0,0);(-1,0);(-1,-1);(0,2);(-1,2)]);
]

let init_tetromino pos = function
  | I as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 0);(0,0);(1,0);(2,0)];
      offsets = [
        (Zero, [(0,0);(-1,0);(2,0);(-1,0);(2,0)]);
        (Right, [(-1,0);(0,0);(0,0);(0,1);(0,-2)]);
        (Two, [(-1,1);(1,1);(-2,1);(1,0);(-2,0)]);
        (Left, [(0,1);(0,1);(0,1);(0,-1);(0,2)]);
      ]
    }
  | T as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 0);(0,0);(1,0);(0,1)];
      offsets = standard_offsets
    }
  | S as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 0);(0,0);(0,1);(1,1)];
      offsets = standard_offsets
    }
  | Z as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 1);(0,1);(0,0);(1,0)];
      offsets = standard_offsets
    }
  | L as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 0);(0,0);(1,0);(1,1)];
      offsets = standard_offsets
    }
  | J as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(-1, 1);(-1,0);(0,0);(1,0)];
      offsets = standard_offsets
    }
  | O as ptype -> 
    {
      pos = pos;
      orientation = Zero;
      piece_type = ptype;
      initial_minos_position = [(0, 0);(1,0);(0,1);(1,1)];
      offsets = [
        (Zero, [(0,0)]);
        (Right, [(0,-1)]);
        (Two, [(-1,-1)]);
        (Left, [(-1,0)]);
      ];
    }

let rotate_mino orientation (x, y) = 
  match orientation with
  | Left -> (-y, x)
  | Two -> (-x, -y)
  | Right -> (y, -x)
  | Zero -> (x, y)

let rotate_minos orientation = List.map (rotate_mino orientation)

let add_pos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let minos_world_positions tetromino =
  tetromino.initial_minos_position 
  |> rotate_minos tetromino.orientation
  |> List.map (add_pos tetromino.pos)

let change_orientation orientation tetromino = 
  { tetromino with orientation = orientation }

let change_pos pos tetromino = 
  { tetromino with pos = pos }

let move delta_pos tetromino =
  { tetromino with pos = Utils.add_pos (pos tetromino) delta_pos }
