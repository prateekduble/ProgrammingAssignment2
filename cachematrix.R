## The program consists of two functions:
## 1. makeCacheMatrix and 2. cacheSolve

## the first function provides functions that work on a matrix and provides
## functions to a) save matrix b) retrieve matrix c) save inverse of
## matrix and d) retrieve inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) matrixInverse <<- solve
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function takes a matrxi as an argument and looksup in the cache
## If found in the cache, it retursn the saved inverse. if the inverse is
## not found in the cache, the inverse is calculated and stored in the cache
## before finally returning the value of the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse of matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- makeCacheMatrix(data, ...)
  x$setInverse(inverse)
  inverse
}
