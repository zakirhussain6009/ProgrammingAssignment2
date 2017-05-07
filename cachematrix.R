## The first function take the Matrix which is inversible and second function gives
## cached inverse matrix if already exists otherwise calculates the inverse of Matrix

## This Function helps Creating / Storing a Matrix and also stores Inverse of a Matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(invers) inv <<- invers
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gets the stored Inverse Matrix otherwise Calculates the Inverse of
## already Stored Matix.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached Inverse Matrix")
    return(m)
  }
  a <- x$getMatrix()
  m <- solve(a)
  x$setInverse(m)
  m
}
