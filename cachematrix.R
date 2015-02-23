## these functions are used to find the inverse of a given matrix and store
## the value in cache, such that future function calls will pull the inverse
## from cache, as opposed to recomputing the inverse

## this function creates a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL    
  }
  get <- function() x
  setinverse <- function(inverse2) inverse <<- inverse2
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function checks to see if the matrix inverse has already been computed,
## if so, it gets the inverse from cache, otherwise it computes the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x, ...)
  x$setinverse(inverse)
}
