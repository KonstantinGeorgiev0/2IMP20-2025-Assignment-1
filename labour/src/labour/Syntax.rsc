module labour::Syntax

// define whitespace layout
layout Whitespace = [\t\n\r\ ]*;

/*
 * Define a concrete syntax for LaBouR. The language's specification is available in the PDF (Section 2)
 */

/*
 * Note, the Server expects the language base to be called BoulderingWall.
 * You are free to change this name, but if you do so, make sure to change everywhere else to make sure the
 * plugin works accordingly.
 */
start syntax BoulderingWall
 = "bouldering_wall" Identifier "{" Volumes "," Routes "}" 
 // identify wall with name; contains both Volumes and Routes and ensures both are present
 ;

// define volumes section surrounded by square bracket and define one or more volumes allowed, separated by comma
syntax Volumes = "volumes" "[" Volume ("," Volume)* "]";

// define volume options
syntax Volume = Circle | Rectangle | Polygon;

// define circle volume
syntax Circle = 
    "circle" "{" Position "," Depth "," Radius "}";

// define rectangle volume and add optional comma for holds
syntax Rectangle = 
    "rectangle" "{" Position "," Depth "," Width "," Height HoldsOpt? "}";

// define polygon volume and add optional comma for holds 
syntax Polygon = 
    "polygon" "{" Position "," Faces "}";

// define faces and vertices
syntax Faces = 
    "faces" "[" Face ("," Face)* "]";
    
// optional comma to start holds
syntax Face = 
    "face" "{" Vertices "," Holds "}"
    |   "face" "{" Vertices "}"
    ;
// faces have exactly 3 vertices
syntax Vertices = 
    "vertices" "[" Vertex "," Vertex "," Vertex "]";
// each vertex has x,y,z coordinates
syntax Vertex =
    "{" "x" ":" Integer "," "y" ":" Integer "," "z" ":" Integer "}";

// define shared volume properties
syntax Position = "pos" "{" "x" ":" Integer "," "y" ":" Integer "}";
syntax Depth = "depth" ":" Integer;
syntax Radius = "radius" ":" Integer;
syntax Width = "width" ":" Integer;
syntax Height = "height" ":" Integer;

// holdsopt definition as possible to have ","
syntax HoldsOpt = "," Holds;
// holds contain one or more hold objects separated by comma
syntax Holds = 
    "holds" "[" Hold "]"
    |   "holds" "[" Hold ("," Hold)+ "]"
    ;
syntax Hold = 
    "hold" HoldID "{" HoldProperties "}";

// define hold properties 
syntax HoldProperties = HoldProp ("," HoldProp)*;
syntax HoldProp =
      "pos" Position2D
    | "shape" ":" Shape
    | "rotation" ":" Integer
    | "colours" "[" Colour ("," Colour)* "]"
    | "start_hold" ":" Integer
    | "end_hold";

// define 2d pos
syntax Position2D = "{" "x" ":" Integer "," "y" ":" Integer "}";

// define Routes with possible multiple routes separated by comma
syntax Routes =
    "routes" "[" BoulderingRoute "]"
    | "routes" "[" BoulderingRoute ("," BoulderingRoute)+ "]"
    ;

// define route structure
syntax BoulderingRoute =
    "bouldering_route" Identifier "{" RouteProperties ("," RouteProperties)* "}";

// define route properties
syntax RouteProperties =
      "grade" ":" Grade
    | "grid_base_point" Position2D
    | "holds" "[" Identifier ("," Identifier)* "]";

// define lexical rules

// Grade is a string
// Identifier is a string
// Shape is a string

lexical Integer = integer : "-"?[0-9]+;
// lexical Integer = [0-9]+;
lexical Identifier = identifier: "\"" ![\"]* "\"";
// lexical Identifier = "\"" ![\"]* "\"";
lexical Shape = Shape: "\"" ![\"]* "\"";
// lexical Shape = "\"" ![\"]* "\"";
lexical Grade = grade: "\"" ![\"]* "\"";
// lexical Grade = "\"" ![\"]* "\"";
lexical HoldID = "\"" [0-9][0-9][0-9][0-9] "\"";
// lexical Colour = "white" | "yellow" | "green" | "blue" | "red" | "purple" | "pink" | "black" | "orange";
lexical Colour = colour:  "white" | "yellow" | "green" | "blue" | "red" | "purple" | "pink" | "black" | "orange";
