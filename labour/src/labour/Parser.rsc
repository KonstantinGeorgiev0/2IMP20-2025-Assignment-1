module labour::Parser

import ParseTree;
import IO;
import labour::Syntax;
import Exception;

/*
 * We already provided the parser for the LaBouR language. The name of the function must be parseLaBouR.
 * This function receives as a parameter the path of the file to parse represented as a loc, and returns a parse tree
 * that represents the parsed program.
 */

start[BoulderingWall] parseLaBouR(loc filePath) {
    try {
        return parse(#start[BoulderingWall], readFile(filePath));
    }
    catch ParseError(e): {
        println("Syntax error while parsing <e>");
        throw e;
    }    
}
