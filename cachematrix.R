## Functions that cache the inverse of a matrix

## store (matrix,inverse-matrix) pairs for quick access
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## quickly return matrix's inverse using cached information
cacheSolve <- function(x, ...) {
  # get current inverse of X
  i <- x$getinverse()
  # if not NULL, it has been previously set; return 
  if (!is.null(i)) {
    message("getting cashed data")
    return(i)
  }
  # else, compute and store in cache
  data <- x$get()
  i <- solve(data, ...) # solve(X) returns its inverse
  x$setinverse(i)
  i
}
