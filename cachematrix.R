## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object with methods for assignment and getting
## of the matrix proper, and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(x) inv <<- x
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieves the matrix inverse if available, or computes and caches it for later.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Test:
# x <- makeCacheMatrix(matrix(1:4, nrow=2))
# cacheSolve(x)
# cacheSolve(x)

