module labour::AST

import util::Maybe;

/*
 * Define the Abstract Syntax for LaBouR
 * - Hint: make sure there is an almost one-to-one correspondence with the grammar in Syntax.rsc
 */

data BoulderingWall(loc src=|unknown:///|)
  = boulderingWall(str name, list[Volume] Volumes, list[BoulderingRoute] routes) 
  // define string name, volume and route options as contents of the wall
  ;

// define volume options
data Volume(loc src=|unknown:///|)
  = circle( Position pos, int depth, int radius )
  | rectangle( Position pos, int depth, int width, int height, list[Hold] holds )
  | polygon( Position pos, list[Face] faces )
  ;

// define additional types
data Position = pos( int x, int y );
// 3d point
data Vertex = vertex( int x, int y, int z);
// polygon face with vertices and holds
data Face( loc src=|unknown:///| ) =
  face(list[Vertex] vertices, list[Hold] holds)
  ;

// define holds
data Hold(loc src=|unknown:///|) =
  hold( str id, Position pos, str shape, list[Colour] colours, Maybe[int] rotation, Maybe[int] start_hold, bool end_hold )
  ;

// define routes
data BoulderingRoute(loc src=|unknown:///|) =
  route(str id, str grade, Position grid_base_point, list[str] holdIds )
  ;

// colour
data Colour =
    white() | yellow() | green() | blue() | red()
  | purple() | pink() | black() | orange();
