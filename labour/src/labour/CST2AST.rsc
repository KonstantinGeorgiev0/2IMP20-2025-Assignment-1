module labour::CST2AST

// This provides println which can be handy during debugging.
import IO;

// These provide useful functions such as toInt, keep those in mind.
import Prelude;
import String;
import ParseTree;
import Node;

import labour::AST;
import labour::Syntax;

import util::Maybe;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 * Hint: Use switch to do case distinction with concrete patterns
 * Map regular CST arguments (e.g., *, +, ?) to lists
 * Map lexical nodes to Rascal primitive types (bool, int, str)
 */
public BoulderingWall cst2ast(Tree cst) {
    // Temporary holders
    str name = "";
    list[Volume] volumes = [];
    list[BoulderingRoute] routes = [];

    switch (cst) {
        // match root BoulderingWall node
        case appl(
            prod(sort("BoulderingWall"), _, _), 
            children
            ): {
                // Iterate over each direct child of the wall
                for (Tree child <- children) {
                    switch (child) {
                        // identifier subtree carries the wall name
                        case appl(
                            prod(label("identifier", lex("Identifier")), _, _), _):
                            {
                                name = getString(child);  
                                println("identifier success");   
                            }

                        case appl(
                            // volumes subtree where we collect all volume types
                            prod(sort("Volumes"), _, _), volumeChildren):
                            {
                                println("volume in cst2ast reached.");
                                volumes = getVolumes7(volumeChildren);
                                println("success vovovovovo");
                            }

                        // routes subtree—collect all BoulderingRoute nodes
                        case appl(
                                prod(sort("Routes"), _, _), routeChildren):
                                {
                                    println("routesss");
                                    routes = getRoutes(routeChildren);
                                    println("routes success");
                                }

                        // all else is ignored
                        default: ;
                    }
                }

                // build AST node once we have name, volumes, and routes
                return boulderingWall(name, volumes, routes);
            }
        default:
        throw "Invalid BoulderingWall structure: " + toString(cst);
    }
}

// note the lowercase constructor boulderingWall
public bool hasVolumes(boulderingWall(_, vols, _)) = size(vols) > 0;

public bool hasRoutes(boulderingWall(_, _, rts)) = size(rts) > 0;

// private Tree stripLayout(Tree t) {
//     switch (t) {
//         // if Rascal-wrapper around start symbol,
//         case appl(prod(label("start", sort(_)), _, _), [ inner ]):
//             return stripLayout(inner);
//         // if pure layout node
//         case appl(prod(label("layouts", _), _, _), _):
//             throw "unreachable";
//         default:
//             return t;
//     }
// }

// strip the surrounding quotes helper function
private str stripQuotes(str s) {
    if (size(s) >= 2 
        && s[0] == "\""          // compare to the string containing a single quote character
        && s[size(s)-1] == "\"") 
    {
        return substring(s, 1, size(s)-1);
    }
    else {
        return s;
    }
}

// pull out all immediate children of an appl node
private list[Tree] getListFromSeq(Tree t) {
    switch (t) {
        case appl(_, children):
            // list comprehension guard uses comma, and children is the list bound above
            return [ c | c <- children, isAppl(c) ];
        default:
            return [];
    }
}


private bool isAppl(Tree t) {
    switch(t) { 
        case appl(_, _): 
            return true;
        default: 
            return false; 
    }
}

// public str getString(Tree t) {
//     switch (t) {
//         // Identifier is a lexical non terminal
//         case appl(
//             prod(label("identifier", lex("Identifier")), _, _), 
//             [ _openQuote, content, _closeQuote ]
//             ):
//             return stripQuotes(toString(content));
//         // fawback for any raw identifier
//         case appl(prod(lex("Identifier"), _, _), [ child ]):
//             return stripQuotes(toString(child));
//         default:
//             throw "Expected Identifier, got: " + toString(t);
//     }
// }

public str getString(Tree t) {
  // reconstruct the original concrete syntax (including quotes)
  str text = unparse(t);
  // strip leading/trailing quotes
  return stripQuotes(text);
}


// private int getInt(Tree t) {
//     switch (t) {
//         // Integer is a lexical non terminal
//         // case appl(prod(label("Integer", _), _, _), [ child ]):
//         case appl(prod(lex("Integer"), _, _), [ child ]):
//             return toInt(stripQuotes(toString(child)));
//         default:
//             throw "Expected Integer, got: " + toString(t);
//     }
// }
private int getInt(Tree t) {
    // unparse entire subtree
    // strip quotes if any, and convert to int
    str text = stripQuotes(unparse(t));
    return toInt(text);
}

private list[Volume] getVolumes7(list[Tree] children) {
    list[Volume] result = [];
    // get Volume wrapper, then for each inner Volume node call getVolume
    for (appl(prod(sort("Volume"), _, _), inner) <- children) {
        for (Tree v <- inner) {
        //   println("extracting volume node: " + toString(v));
        result += getVolume(v);
        }
    }
    return result;
}

// private list[Volume] getVolumes6(list[Tree] children) {
//     println("getVolumes function reached.");
//     // println(children);
//     // first volume is the single Volume node at index 4
//     list[Volume] first  = [ getVolume(v) 
//                             | v <- getListFromSeq(children[4]) ];

//     // the remaining volumes sit under the iter-star-seps node at index 6
//     list[Volume] rest   = [ getVolume(v) 
//                             | v <- getListFromSeq(children[6]) ];
//     return first + rest;
// }

// private list[Volume] getVolumes6(list[Tree] children) {
//   println("=== getVolumes6 reached ===");
//   list[Volume] result = [];

//   for (Tree c <- children) {
//     println("  child: " + toString(c));

//     visit(c) {
//         case v: appl(prod(label("circle", _), _, _), _) {
//             println("    found circle: " + toString(v));
//             result += getVolume(v);
//         }
//         // case a: appl(prod(label("rectangle", _), _, _), _) {
//         //     println("    found rectangle: " + toString(v));
//         //     result += getVolume(a);
//         // }
//         // case b: appl(prod(label("polygon",   _),_,_),_) {
//         //     println("    found polygon: " + toString(v));
//         //     result += getVolume(b);
//         // }
//         default: ;
//     }
//   }

//   println("=== getVolumes6 result count = " + toString(size(result)) + " ===");
//   return result;
// }


// // takes the list of direct children under the Volumes node
// private list[Volume] getVolumes(list[Tree] children) {
//     list[Volume] result = [];
//     println("getVolumes function reached.");

//     for (Tree child <- children) {
//         visit(child) {
//         // catch any polygon node anywhere under child
//         case p: appl(prod(label("polygon", _), _, _), _):
//             result += getVolume(p);

//         // circle
//         case c: appl(prod(label("circle", _), _, _), _):
//             result += getVolume(c);
        
//         // rectangle
//         case r: appl(prod(label("rectangle", _), _, _), _):
//             result += getVolume(r);
//         }
//     }
//     // println(result);
//     return result;
// }

private Volume getVolume(Tree t) {
    // println("getVolume function reached for: " + toString(t));
    println("getVolume function reached.");    
    // println(t);

    switch (t) {
        case appl(prod(label("circle", _), _, _), [_, _, pos, _, depth, _, radius, _]):
        {
            println("cicic");
            return circle(getPosition(pos), getInt(depth), getInt(radius));
        }

        case appl(prod(label("rectangle", _), _, _), [_, _, pos, _, depth, _, width, _, height, maybeHolds]):
        {
            println("recrtec");
            return rectangle(getPosition(pos), getInt(depth), getInt(width), getInt(height), getHoldsOpt(maybeHolds));
        }

        case appl(
            prod(sort("Polygon"), _, _),
            [
                _,       // 0: “polygon” literal
                _,       // 1: layouts
                _,       // 2: “{”
                _,       // 3: layouts
                posNode, // 4: Position subtree
                _,       // 5: layouts
                _,       // 6: “,”
                _,       // 7: layouts
                facesNode, // 8: Faces subtree
                _,       // 9: layouts
                _        // 10: “}”
            ]
            ):
            {
                println("polyyyyaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
                println(posNode);
                println(facesNode);
                return polygon(
                    getPosition(posNode),
                    getFaces(facesNode)
                );
            }
    }
    throw "Unknown volume type";
}

private Position getPosition(Tree t) {
    // println("getPosition reached with: " + toString(t));
    list[Tree] children = getListFromSeq(t);
    // println("CHILDREEEEEN: " + children);

    // collect all integer children under this Position node
    list[int] coords =
        [ getInt(c)
        | c : appl(prod(label("integer", _), _, _), _) <- children
        ];
    // println("COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO" + coords);
    if (size(coords) == 2) {
        return pos(coords[0], coords[1]);
    }
    else {
        throw "Expected two integers in Position, got: " + toString(t);
    }
}

// private list[Face] getFaces(Tree t) {
//     println("getFaces reached with: " + toString(t));
//     switch (t) {
//         case appl(prod(sort("Faces"), _, _), children): {
//             // the very first Face is at position 4
//             Face first = getFace(children[4]);

//             // remaining Face nodes live under the iter-star-seps at position 6
//             // list[Tree] reps  = getListFromSeq(children[6]);
//             // list[Face] rest = [ getFace(f) | f <- reps ];
//             list[Tree] more = getListFromSeq(children[6]);
//             return [ first ] + [ getFace(f) | f <- more ];

//             // return [ first ] + rest;
//         }
//         default:
//         throw "Expected Faces node, got: " + toString(t);
//     }
// }

private list[Face] getFaces(Tree t) {
    // println("getFaces reached with: " + toString(t));
    list[Face] faces = [];
    visit(t) {
        // whenever we hit a Face application, bind it to f and convert it
        case f: appl(prod(sort("Face"), _, _), _) :
            faces += getFace(f);
        // default: 
        //     throw "Expected Faces list, got: " + toString(t);
    }
    if (size(faces) > 0) {
        println("getFaces result: " + toString(faces));
        return faces;
    }
    else {
        throw "Expected at least one Face, got: " + toString(t);
    }
    // return faces;
}

// private Face getFace(Tree t) {
//     // println("getFace reached with: " + toString(t));

//     switch (t) {
//         case appl(prod(sort("Face"), _, _), children):
//         {
//             // pick out the Vertices and Holds subtrees
//             list[Tree] verts = [ v | v : appl(prod(sort("Vertices"), _, _), _) <- children ];
//             list[Tree] holds = [ h | h : appl(prod(sort("Holds"),    _, _), _) <- children ];

//             return face(
//                 getVertices(verts[0]),
//                 size(holds) > 0 ? getHoldsOpt(holds[0]) : []
//             );
//         }
//         default:
//             throw "Expected Face, got: " + toString(t);
//     }
// }

private Face getFace(Tree f) {
    return face(
        getVertices(f),   // grab all Vertex nodes under this face
        getHolds(f)       // grab all Hold nodes under this face
    );
}

private list[Vertex] getVertices(Tree t) {
    println("getVertices reached with: " + toString(t));
    list[Vertex] vs = [];
    visit(t) {
        // catch CST node for a vertex
        // case appl(prod(sort("Vertex"), children, _)):
        case appl(
           prod(sort("Vertex"), _, _),   
           subtrees
        ):
        {
            println("found a Vertex node with children: " + toString(subtrees));

            // extract all lex("Integer") leaves from those subtrees
            list[int] coords = [
                getInt(iLeaf)
                | appl(prod(lex("Integer"), _, _), [ iLeaf ]) <- subtrees
            ];
            if (size(coords) == 3) {
                vs += vertex(coords[0], coords[1], coords[2]);
            }
        }
        default:
            throw "Expected list of Vertices.";
    }
    return vs;
}

// private list[Vertex] getVertices(Tree t) {
//     // t is the CST node for “Vertices [… ]”
//     println("getVertices reached with: " + toString(t));
//     list[Vertex] result = [];

//     // get all the immediate appl-children under the Vertices node
//     for (Tree v <- getListFromSeq(t)) {
//         // collect exactly the three integer arguments inside v
//         println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv: " + toString(v));
//         list[Tree] coords =
//         [ nc
//             | appl(prod(label("integer",lex("Integer")), _, _), [nc])  
//             <- getListFromSeq(v)
//         ];

//         if (size(coords) != 3) {
//         throw "Expected exactly three Integer children in Vertex, got: " 
//                 + toString(v);
//         }

//         result += vertex(
//         getInt(coords[0]),
//         getInt(coords[1]),
//         getInt(coords[2])
//         );
//         // // v should be an appl(prod(sort("Vertex"),_,_), children)
//         // // pull out exactly that list of children
//         // list[Tree] children = getListFromSeq(v);
//         // println("DUDDUEDUDUEUDEUDEUDUEAUDEUASDUASDUASDUASDUAS");
//         // println(toString(children));
//         // // by convention our grammar for Vertex is:
//         // //   "{" WS xLit WS "," WS yLit WS "," WS zLit WS "}"
//         // // so xLit is at index 2, yLit at index 6, zLit at index 10
//         // int x = getInt(children[2]);
//         // int y = getInt(children[6]);
//         // int z = getInt(children[10]);

//         // result += vertex(x, y, z);
//     }

//     return result;
// }


private list[Hold] getHolds(Tree t) {
  list[Hold] hs = [];
  // find every Hold node anywhere under t
  visit (t) {
    case h: appl(prod(label("hold", _), _, _), _) :
      hs += getHold(h);
  }
  return hs;
}

// private list[Vertex] getVertices(Tree t) =
//     [vertex(getInt(x), getInt(y), getInt(z)) 
//         | appl(_, [x, y, z]) <- getListFromSeq(t)
//     ];

private list[Hold] getHoldsOpt(Tree t) {
    switch (t) {
        case appl(_, [_, _, holdList]): return [getHold(h) | h <- getListFromSeq(holdList)];
        default: return [];
    }
}

private Hold getHold(Tree t) {
    switch (t) {
        case appl(prod(label("hold", _), _, _), [_, id, _, _, propList, _]): {
            str hid = getString(id);
            Position p = pos(0, 0);
            str shape = "";
            list[Colour] colours = [];
            Maybe[int] rotation = nothing();
            Maybe[int] startHold = nothing();
            bool endHold = false;

            for (Tree prop <- getListFromSeq(propList)) {
                switch (prop) {
                    // case appl(prod(label("pos", _), _, _), [_, _, posT]):
                    case appl(prod(label("pos", _), _, _), [_, posT]): 
                        p = getPosition(posT);
                    case appl(prod(label("shape", _), _, _), [_, _, shapeT]): 
                        shape = getString(shapeT);
                    case appl(prod(label("colours", _), _, _), [_, _, _, _, cList]): 
                        colours = [getColour(c) | c <- getListFromSeq(cList)];
                    case appl(prod(label("rotation", _), _, _), [_, _, r]): 
                        rotation = just(getInt(r));
                    case appl(prod(label("start_hold", _), _, _), [_, _, s]): 
                        startHold = just(getInt(s));
                    case appl(prod(label("end_hold", _), _, _), _): 
                        endHold = true;
                }
            }

            return hold(hid, p, shape, colours, rotation, startHold, endHold);
        }
        default:
            throw "Invalid hold.";
    }
}

// private list[BoulderingRoute] getRoutes(Tree t) = [getRoute(r) | r <- getListFromSeq(t)];

// children is the list[Tree] under the Routes node
private list[BoulderingRoute] getRoutes(list[Tree] children) {
    list[BoulderingRoute] result = [];
    for (Tree c <- children) {
        visit(c) {
        case r: appl(prod(label("bouldering_route", _), _, _), _):
            result += getRoute(r);
        default: ;
        }
    }
    return result;
}


private BoulderingRoute getRoute(Tree t) {
    switch (t) {
        case appl(prod(label("bouldering_route", _), _, _), [_, id, _, _, propList, _]): {
            str rid = getString(id);
            str grade = "";
            Position gbp = pos(0, 0);
            list[str] holdIds = [];

            for (Tree prop <- getListFromSeq(propList)) {
                switch (prop) {
                    case appl(prod(label("grade", _), _, _), [_, _, g]): 
                        grade = getString(g);
                    // case appl(prod(label("grid_base_point", _), _, _), [_, _, p]): 
                    case appl(prod(label("grid_base_point", _), _, _), [_, p]):  
                        gbp = getPosition(p);
                    case appl(prod(label("holds", _), _, _), [_, _, idList]): 
                        holdIds = [getString(h) | h <- getListFromSeq(idList)];
                    default:
                        ;
                }
            }
            return route(rid, grade, gbp, holdIds);
        }
        default:
            throw "Invalid bouldering_route node.";
    }
}

private Colour getColour(Tree t) {
  switch (t) {
    case appl(prod(label("Colour", _), _, _), [ child ]):
      switch (stripQuotes(toString(child))) {
        case "white":  return white();
        case "yellow": return yellow();
        case "green":  return green();
        case "blue":   return blue();
        case "red":    return red();
        case "purple": return purple();
        case "pink":   return pink();
        case "black":  return black();
        case "orange": return orange();
        default:
          throw "Invalid colour literal: " + toString(child);
      }
    default:
      throw "Expected Colour, got: " + toString(t);
  }
}
