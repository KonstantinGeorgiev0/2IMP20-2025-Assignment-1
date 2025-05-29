module labour::Check

import labour::AST;
import labour::Parser;
import labour::CST2AST;

import IO;
import List;
import Prelude;
import String;



/*
 * Implement a well-formedness checker for the LaBouR language. For this you must use the AST.
 * - Hint: Map regular CST arguments (e.g., *, +, ?) to lists
 * - Hint: Map lexical nodes to Rascal primitive types (bool, int, str)
 * - Hint: Use switch to do case distinction with concrete patterns
 */

/*
 * Define a function per each verification defined in the PDF (Section 2.2.)
 * Some examples are provided below.
 */

bool checkBoulderWallConfiguration(BoulderingWall wall){
  return  checkWallVolumesAndRoutes(wall)          
       && checkNumberOfHolds(wall)                 
       && checkStartingHoldsTotalLimit(wall)       
       && checkRoutesHaveMandatoryFields(wall)     
       && checkUniqueEndHold(wall)                 
       && checkHoldIdFormat(wall)                  
       && checkRouteColourConsistency(wall)        
       && checkHoldsWellFormed(wall)               
       && checkVolumeValidity(wall);               
}

//  Check if the the wall hast more than one volume and route 
bool checkWallVolumesAndRoutes(BoulderingWall wall){
  if(isEmpty(wall.Volumes) || isEmpty(wall.routes)){
    return false;
  }else{
    return true;
  }
}


// Check that there are at least two holds in the wall
bool checkNumberOfHolds(BoulderingWall wall) {
  for (route(_, _, _, holdIds) <- wall.routes) {
    if (size(holdIds) < 2) {
      return false;
    }
  }
  return true;
}

// Check that route must have non-empty id & grade, and a Position with x & y
bool checkRoutesHaveMandatoryFields(BoulderingWall wall) {
  for (route(id, grade, pos(_, _), _) <- wall.routes) {
    if (trim(id) == "" || trim(grade) == "") {
      return false;
    }
  }
  return true;
}

// This function will insure that there is only one hold assign to end hold
bool checkUniqueEndHold(BoulderingWall wall){
  map[str, Hold] holdsMap = createHoldMap(wall);               
  for(route(_,_,_, holdIds) <- wall.routes){
    int count = 0;
    for (hid <- holdIds){
      if(hid in holdsMap && holdsMap[hid].end_hold){           
        count += 1;
      }
      if(count > 1){
        return false;
      }
    }
  }
  return true;
}

// Check for correct format 4 digits. Wall and id are free format
bool checkHoldIdFormat(BoulderingWall wall) {
  for (h <- getAllHolds(wall)){
    if (!isFourDigit(h.id)) {
      return false;
    }
  }
  return true;
}

// helper function for checking the 4 digit format
bool isFourDigit(str s) = /^\d{4}$/ := s;

// Check if a route has the same color( milti color is allowed if at least one color mathches the one of the route)
bool checkRouteColourConsistency(BoulderingWall wall) {
  map[str, Hold] holdMap = createHoldMap(wall);                

  for (route(_, _, _, holdIds) <- wall.routes) {
    if (isEmpty(holdIds)) {
      continue;
    }        
    str firstId = holdIds[0];
    if (!(firstId in holdMap)) {
      return false;
      }

    list[Colour] routeCols = holdMap[firstId].colours;
    if (isEmpty(routeCols)) {
      return false;
    }

    // Decide route colour arbitrarily as the HEAD of that list
    Colour routeColour = routeCols[0];

    for (hid <- holdIds) {
      if (hid in holdMap) {
        list[Colour] cols = holdMap[hid].colours;
        if (!(routeColour in cols)) {
          return false;
        }
      }
    }
  }
  return true;
}

// Check that each hold has pos/shape/colour and valid rotation & colour
bool checkHoldsWellFormed(BoulderingWall wall) {
  for (hold(_, pos(_, _), shape, colours, rot, _, _) <- getAllHolds(wall)) {

    // shape name must be non-blank
    if (trim(shape) == "") {
      return false;
    }

    // each hold needs at least one colour
    if (isEmpty(colours)) {
      return false;
    }

    // rotation, when present, must be in the range 0 to 359°
    switch (rot) {                                          
      case just(int deg):
        if (deg < 0 || deg >= 360) {
          return false;
        }
      default:
        ; // nothing
    }
  }
  return true;
}

// Check that routes have between zero and two hand start holds
bool checkStartingHoldsTotalLimit(BoulderingWall wall) {
  map[str, Hold] holdsMap = createHoldMap(wall);

  for(route(id, _, _, holdIds) <- wall.routes){
    int count = 0;
    for(hid <- holdIds){
      if(hid in holdsMap && holdsMap[hid].start_hold != nothing){
        count +=1;
      }
    }
    if (count >2){
      return false;
    }
  }  
  return true;
}

bool checkVolumeValidity(BoulderingWall wall) {
  for (v <- wall.Volumes) {
    switch (v) {
      case circle(_, depth, radius):{

        if (depth <= 0 || radius <= 0) {
          return false;
          }
      }
      case rectangle(_, depth, width, height, _):{

        if (depth <= 0 || width <= 0 || height <= 0) {
          return false;
        }
      }
      case polygon(_, faces):{

        if (isEmpty(faces)){
          return false;
        }

        for (face(vertices, _) <- faces){
          if (size(vertices) != 3){ 
            return false; 
          }
        }
      }
    }
  }
  return true;
}

// Helper functions
list[Hold] getAllHolds(BoulderingWall wall) =
  [ h | rectangle(_,_,_,_,hs) <- wall.Volumes, h <- hs ]  
  +
  [ h | polygon(_,fs) <- wall.Volumes
      , face(_,hs)    <- fs
      , h             <- hs ];

map[str,Hold] createHoldMap(BoulderingWall wall){
  map[str, Hold] m = ();
  for (h <- getAllHolds(wall)){
    m[h.id] = h;
  }
  return m;
}