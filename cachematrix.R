## The following are a pair of functions that cache the inverse of a matrix.
## Matrix inversion is a costly computation and there is a benefit to caching the inverse
## rather than computing it multiple times.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  seti <- function(i) inverse <<- i
  geti <- function() i
  list(set = set, get = get, 
       seti = seti, geti = geti)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$geti()
  if(!is.null(inverse)) {
    message("Getting chached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$seti(inverse)
  inverse
}
