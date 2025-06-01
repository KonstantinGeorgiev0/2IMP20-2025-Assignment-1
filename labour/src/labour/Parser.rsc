module labour::Parser

import ParseTree;
import IO;
import labour::Syntax;
import Exception;

/*
 * We already provided the parser for the LaBouR language. The name of the function must be parseLaBouR.
 * This function receives as a parameter the path of the file to parse represented as a loc, and returns a parse tree
 * that represents the parsed program.
 * The original implementation parsed the input using the production #start[BoulderingWall] and that choice produces a 
 * parse tree whose root node is this start label, which has zero semantic value for the language itself in our opinion. 
 * Also, we had some issues in the CST to AST transformation where we have to peel off this extra layer, so we decided to 
 * parse it directly with #BoulderingWall. 
 */
public BoulderingWall parseLaBouR(loc filePath) {
   try {
      // parse directly with the grammar start non‑terminal.
      return parse(#BoulderingWall, readFile(filePath));
   } catch ParseError(e): {
      println("Syntax error while parsing: <e>");
      throw e;
   }
}