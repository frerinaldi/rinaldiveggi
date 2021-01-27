Pawn <- setRefClass("Pawn",
                    fields=list(
                      color = "character", 
                      isPiece = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isPiece <<- FALSE
                        }}
                      ,
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, currentColor, cb) {
                        #black, from the bottom
                        if (cb[source$row, source$col][[1]]$color == "b") {
                          if (source$row==7) { 
                            if ((destination$row-source$row) == 2 || (destination$col == source$col)) {
                              return(TRUE)
                            }}
                          if ((destination$row-source$row) == -1) {
                            #pawn - eat
                            if (((is(chessboard[destination$row, destination$col+1][[1]], "Pawn") ==TRUE) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "King") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Rook") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Knight") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Queen") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Bishop") ==TRUE)) ||  
                                 ((is(chessboard[destination$row, destination$col-1][[1]], "Pawn") ==TRUE) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "King") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Rook") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Knight") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Queen") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Bishop") ==TRUE))) ||
                                 (cb[destination$row, destination$col][[1]]$color != currentColor))) { 
                              if (abs(destination$col-source$col)<=1) {
                                return(TRUE)
                              } #default move
                            }
                          }}
                        #white, from the top
                        if (cb[source$row, source$col][[1]]$color == "w") {
                          if (source$row==2) { 
                            if ((destination$row-source$row) == 2 || (destination$col == source$col)) {
                              return(TRUE)
                            }}
                          if ((destination$row-source$row) == 1) {
                            #pawn - eat
                            if (((is(chessboard[destination$row, destination$col+1][[1]], "Piece") ==TRUE) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "King") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Rook") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Knight") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Queen") ==TRUE)) || 
                                 ((is(chessboard[destination$row, destination$col+1][[1]], "Bishop") ==TRUE)) ||  
                                 ((is(chessboard[destination$row, destination$col-1][[1]], "Piece") ==TRUE) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "King") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Rook") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Knight") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Queen") ==TRUE)) || 
                                  ((is(chessboard[destination$row, destination$col-1][[1]], "Bishop") ==TRUE))) &&
                                 (cb[destination$row, destination$col][[1]]$color != currentColor)) &&
                                 (abs(destination$col-source$col)=1) ) { 
                                return(TRUE)
                              } #default move
                            else if (source$col == destination$col) {
                            return(TRUE)
                          }}
                        return(FALSE)
                      }}
                    ))