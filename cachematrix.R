## Put comments here that give an overall description of what your
## functions do

## Create list containing function to set and get Matrix, set and get inversed Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(matrix) m <<- matrix
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse)
}

## Function checks cache to see whether inverse matrix has been calculated. 
## If calculated, return cached matrix, else calculate matrix and store in cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  data <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
