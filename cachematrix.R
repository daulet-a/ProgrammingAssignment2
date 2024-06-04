## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix with a list of functions to set and get the
## values of matrix, set and get the values of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## It computes the inverse of the matrix from makeCacheMatrix.
## If inverse is already calculated, then cacheSolve retrieves it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

