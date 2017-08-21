## Function description:  Creates a data structure and functionallity (methods) to calculate the Inverse of a matrix or retrieve
## a previously calculated inverse value.

makeCacheMatrix <- function(x = matrix()) {
     # ---------- Initialize values ---------- #
     m <- NULL
     
     # ---------- Sub-methods ---------- #
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     
     setSolve <- function(solve) m <<- solve
     
     getSolve <- function() m
     
     # ---------- Return Value ---------- #
     list(set = set, get = get,
          setSolve = setSolve,
          getSolve = getSolve
          )
}




# Retrieves a previously calculated Inverse matrix if that value exists, or calculates it if it does not exist and sets
# it's value for later references.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getSolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setSolve(m)
     m
}
