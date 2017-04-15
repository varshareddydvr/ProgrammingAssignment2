
## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){       #set the value of the matrix
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x      # get the value of the matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix   # set the value of the inverse
  getInverse <- function() inv          # get the value of the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
