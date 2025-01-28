## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function takes as input a matrix and returns a special object.
## The object has the following methods:
## setMatrix: Can set a new matrix
## getMatrix: returns the matrix stored
## setInverse: sets the inverse if already stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function (y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## CacheSolve returns the inverse of a given matrix, and passes it to the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  m
}
