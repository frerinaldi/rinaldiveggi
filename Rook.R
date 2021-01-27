Rook <- setRefClass("Rook",
                    fields=list(
                      color = "character", 
                      isRook = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        # We want to check if the newColor passed as parameter 
                        # is 'w' or 'b'; If the value is not valid, the 
                        # execution of the software is stopped
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isRook <<- FALSE
                        }
                      },
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        if ((source$col==destination$col) && (abs(source$row-destination$row)>1))  {
                          if (source$row > destination$row) {
                            temp<-source$row
                            source$row<-destination$row
                            destination$row<-temp
                          }
                          for (i in seq (source$row+1, destination$row-1)) {
                            if ((is.null(cb[source$col, i][[1]]) == FALSE)) 
                            {return(FALSE)}}}
                        else if ((source$row==destination$row) && ((destination$col-source$col)>1)) { 
                          for (i in seq (source$col + 1, destination$col - 1)) {
                            if ((is.null(cb[i, source$row][[1]]) == FALSE)) {
                              cat("Trajectory\n")
                              return(FALSE)
                            }}}
                        else if (((source$row==destination$row) && (source$col - destination$col)>1)) {  ####aaaaaaaaaaaaaaaaaa non va a sinistra
                          for (i in seq (destination$col + 1, source$col - 1)) {
                            if ((is.null(cb[i, source$row][[1]]) == FALSE)) {
                              cat("Trajectory\n")
                              return(FALSE)
                            }
                          }
                        }
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, cb, currentColor) {
                        if (((abs(destination$row-source$row)<=8) && (destination$col==source$col)) || (((destination$row==source$row)) && ((abs(destination$col-source$col)<=8 ))) ) {
                          # MOSSA VALIDA
                          return(TRUE)
                        }
                        # MOSSA NON VALIDA
                        cat("Move not allowed\n")
                        return(FALSE)
                      }
                    ))