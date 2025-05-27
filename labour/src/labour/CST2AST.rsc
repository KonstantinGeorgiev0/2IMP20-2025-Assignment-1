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
public BoulderingWall cst2ast(Tree t) {
    // This function converts a concrete syntax tree (CST) to an abstract syntax tree (AST).
    // The CST represents the parsed structure of the code, while the AST represents the logical structure.
    switch (t) {
        // Match the structure of a bouldering wall in the CST
        case appl(prod(label("bouldering_wall", _), _, _), 
            [_, id, _, _, volBlock, _, routeBlock, _]):
            return boulderingWall(
                getString(id), 
                getVolumes(volBlock), 
                getRoutes(routeBlock)
                );
        default:
            throw "Invalid BoulderingWall structure:" + toString(t);
    }
}

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

public str getString(Tree t) {
  switch (t) {
    // Identifier is a lexical non terminal
    case appl(prod(label("Identifier", _), _, _), [ child ]):
      return stripQuotes(toString(child));
    default:
      throw "Expected Identifier, got: " + toString(t);
  }
}

private int getInt(Tree t) {
  switch (t) {
    // Integer is a lexical non terminal
    case appl(prod(label("Integer", _), _, _), [ child ]):
      return toInt(stripQuotes(toString(child)));
    default:
      throw "Expected Integer, got: " + toString(t);
  }
}


private list[Volume] getVolumes(Tree t) = 
    [getVolume(v) | v <- getListFromSeq(t)];

private Volume getVolume(Tree t) {
    switch (t) {
        case appl(prod(label("circle", _), _, _), [_, _, pos, _, depth, _, radius, _]):
        return circle(getPosition(pos), getInt(depth), getInt(radius));

        case appl(prod(label("rectangle", _), _, _), [_, _, pos, _, depth, _, width, _, height, maybeHolds]):
        return rectangle(getPosition(pos), getInt(depth), getInt(width), getInt(height), getHoldsOpt(maybeHolds));

        case appl(prod(label("polygon", _), _, _), [_, _, pos, _, faceList, _]):
        return polygon(getPosition(pos), getFaces(faceList));
    }
    throw "Unknown volume type";
}

private Position getPosition(Tree t) {
    switch (t) {
        case appl(_, [_, _, x, _, _, _, y, _]):
            return pos(getInt(x), getInt(y));
        default: throw "Expected correct position.";
    }
}

private list[Face] getFaces(Tree t) = 
    [getFace(f) | f <- getListFromSeq(t)];

private Face getFace(Tree t) {
    switch (t) {
        case appl(prod(label("Face", _), _, _), [_, vertBlock, maybeHolds]):
            return face(getVertices(vertBlock), getHoldsOpt(maybeHolds));
        default:
            throw "Expected Face, got: " + toString(t);
    }
}

private list[Vertex] getVertices(Tree t) =
    [vertex(getInt(x), getInt(y), getInt(z)) 
        | appl(_, [x, y, z]) <- getListFromSeq(t)
    ];

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
                    case appl(prod(label("pos", _), _, _), [_, _, posT]): 
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

private list[BoulderingRoute] getRoutes(Tree t) = [getRoute(r) | r <- getListFromSeq(t)];

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
                    case appl(prod(label("grid_base_point", _), _, _), [_, _, p]): 
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
