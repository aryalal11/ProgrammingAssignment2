## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse if not cached
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the inverse
  inv
}
