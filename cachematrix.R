##These functions create a matrix where the inverse can be cached and returned
## to save time and computing power.

## Function will create matrix return a list of values used to inverse the 
## matrix

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x<<- y
    c<<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) c <<- inverse
  getmatrix<-function()c
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Function checks to see if there is cached data, and will return it, if no 
## cached data, function will create matrix and return inverse.

cacheSolve <- function(x, ...) {
  c <- x$getmatrix()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  matrix <- x$get()
  c <- solve(matrix, ...)
  x$setmatrix(c)
  c
}
