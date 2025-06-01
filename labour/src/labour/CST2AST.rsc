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

 * Volume types and their faces are extracted successfully.
 * However, vertex/hold data at deeper levels is non-functional
 * and the final `routes` list is still empty.
 */
/*
 * Map the root CST to the root AST node.
 * Walks the immediate children, delegating
 * to helper extractors.
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
                                volumes = getVolumes(volumeChildren);
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

// flags to check if volumes or routes are nonempty
public bool hasVolumes(boulderingWall(_, vols, _)) = size(vols) > 0;

public bool hasRoutes(boulderingWall(_, _, rts)) = size(rts) > 0;

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

// tag test
private bool isAppl(Tree t) {
    switch(t) { 
        case appl(_, _): 
            return true;
        default: 
            return false; 
    }
}

// unparse subtree and srip quotes to get a plain string
public str getString(Tree t) {
    str text = unparse(t);
    // strip leading/trailing quotes
    return stripQuotes(text);
}

// unparse entire subtree, strip quotes and convert to int
private int getInt(Tree t) {
    str text = stripQuotes(unparse(t));
    return toInt(text);
}

// collect all Volume nodes under the Volumes wrapper
private list[Volume] getVolumes(list[Tree] children) {
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

// map one volume type CST to its AST variant
private Volume getVolume(Tree t) {
    // println("getVolume function reached for: " + toString(t));
    println("getVolume function reached.");    
    // println(t);

    switch (t) {
        case appl(
            prod(label("circle", _), _, _), 
            [_, _, pos, _, depth, _, radius, _]
            ):
        {
            println("cicic");
            return circle(getPosition(pos), getInt(depth), getInt(radius));
        }

        case appl(
            prod(label("rectangle", _), _, _), 
            [_, _, pos, _, depth, _, width, _, height, maybeHolds]
            ):
        {
            println("recrtec");
            return rectangle(getPosition(pos), getInt(depth), getInt(width), getInt(height), getHoldsOpt(maybeHolds));
        }

        case appl(
            prod(sort("Polygon"), _, _), 
            [_, _, _, _, posNode, _, _, _, facesNode, _, _]
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

// get 2d posiiton
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

// get all face nodes from a face block
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

// extract vertices and holds from a single face
private Face getFace(Tree f) {
    return face(
        getVertices(f),   // grab all Vertex nodes under this face
        getHolds(f)       // grab all Hold nodes under this face
    );
}

// helper function to get vertices inside a single face
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

// collect hold nodes under subtree t
private list[Hold] getHolds(Tree t) {
  list[Hold] hs = [];
  // find every Hold node anywhere under t
  visit (t) {
    case h: appl(prod(label("hold", _), _, _), _) :
      hs += getHold(h);
  }
  return hs;
}

// optional hold list for the rectangle volume type
private list[Hold] getHoldsOpt(Tree t) {
    switch (t) {
        case appl(_, [_, _, holdList]): return [getHold(h) | h <- getListFromSeq(holdList)];
        default: return [];
    }
}

// get each property of a single hold and pass them onto specific helper functions
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

// collect bouldering route nodes to pass later onto getRoute
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

// match bouldering route properties (grade, gbp, hold regs)
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

// map colors to AST functions
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
