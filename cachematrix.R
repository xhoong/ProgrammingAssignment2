## Functions to store matrix and it's inverse
##
## The two functions below creates an object for storing matrix
## and it's inverse, together with cacheSolve which actually do
## the computation to calculate the inverse matrix.
## Example usage:
##
##   # Create an object to store the matrix
##   m = makeCacheMatrix( rbind(c(10, 20), c(30, 40)) )
##
##   # Calculate the inverse of the matrix,
##   mI1 = cacheSolve(m)
##
##   # Calculating the inverse matrix again will be from the cached results
##   # hence faster performance.
##   mI2 = cacheSolve(m)


## makeCacheMatrix -- creates an object to hold a matrix and a cache
## for its inverse

makeCacheMatrix <- function(x = matrix()) {
  mI <- NULL
  set <- function (y) {
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mI <<- inverse
  getInverse <- function() mI
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve -- Perform matrix inverse computation, takes matrix object
## created from makeCacheMatrix function. If previous computation was done
## the cached value is return. Print message to indicate whether the result
## is from cached value or new computation.
cacheSolve <- function(x, ...) {
  mI <- x$getInverse()
  if(is.null(mI)) {
    message("solving matrix")
    mI <- solve(x$get(), ...)
    x$setInverse(mI)
  } else {
    message("getting cached data")
  }
  mI
}
