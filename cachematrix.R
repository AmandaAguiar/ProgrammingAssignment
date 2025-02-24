## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Setter function to update the matrix and reset inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Setter function to store the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function to retrieve the cached inverse
  getInverse <- function() inv
  
  # Return a list containing all the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse is already calculated and cached, it retrieves it instead of recomputing.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  
  # If cached inverse exists, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
