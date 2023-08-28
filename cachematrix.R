## Author: L. Yang
## This R code contains a set of functions that cache the inverse of a
## matrix. 

## The mackeCacheMatrix function stores a matrix and its inverse by
## using a set of functions and returning them in a list to the parent 
## environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(matInv) m <<- matInv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cacheSolve function will return the inverse of a matrix.
## It will only calculate the inverse if it is not in the 
## the cache. Otherwise it will return the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


