## This funciton creates a "wrapper" around the given matrix with some 
## functions to be able to cache an inversed version of the matrix:
## set(x) - Sets the original matrix and clear the cached inversed matrix
## get()  - Returns the original matrix
## setInverse(m) - Sets the inversed matrix
## getInverse()  - Returns the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  inversedX <- NULL
  set <- function(y) {
    x <<- y
    inversedX <<- NULL
  }
  get <- function() x
  setInverse <- function(m) inversedX <<- m
  getInverse <- function() inversedX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function takes an "cacheMatrix" created by the makeCacheMatrix
## and returns the inversed matrix if present or performs the inversion and
## caches the value by updating the given "cacheMatrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m

}
