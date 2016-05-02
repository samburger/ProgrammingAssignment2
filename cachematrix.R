## These two functions allow the computation of an invertible matrix's inverse.  The makeCacheMatrix function 
## allows for the caching of previously computed inverse values.  cacheSolve will return this inverse if available,
## otherwise the inverse will be computed and stored in the makeCacheMatrix object.

## This function returns a matrix-like object containing methods for getting & setting its data and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of the given matrix x, computing only if not already cached.
## A cache matrix must be passed for argument x as returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("Getting cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
