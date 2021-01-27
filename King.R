King <- setRefClass("King",
                    fields=list(
                      color = "character",
                      isKing = "logical"
                    ),
                    methods=list(
                      initialize = function(newColor) {
                        if ((newColor != "w") && (newColor != "b")) {
                          stop("invalid parameters to the ctor (", newColor, 
                               " but only 'b' and 'w' are valid)")
                        } else {
                          color <<- newColor
                          isKing <<- FALSE
                        }
                      },
                      checkTrajectory=function(source, destination, currentColor, cb) {
                        return (TRUE)
                      },
                      #defines the move this piece can do
                      checkMove=function(source,destination, cb, currentColor) {
                        if (abs(destination$row-source$row)<=1) {
                          #default move of the king
                          if (abs(destination$col-source$col)<=1) {
                            return(TRUE)
                          }
                        }
                        return(FALSE)
                      }
                    ))