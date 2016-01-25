## The following functions solve for and cache the inverse of square matrices.

## makeCacheMatrix creates a list that sets the value of a matrix, gets its value,
## sets the value of its inverse, and gets the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve calculates the inverse of the matrix created above, checking first whether
## the inverse has already been calculated, in which case it skips the computation and gets
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}