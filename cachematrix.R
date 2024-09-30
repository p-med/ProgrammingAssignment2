## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cached inverse when matrix changes
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of all functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If cached, return the cached inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}

# Example of a 2x2 invertible matrix
mat <- matrix(c(1, 2, 3, 4), 2, 2)
myMatrix <- makeCacheMatrix(mat)

cacheSolve(myMatrix)
