## Put comments here that give an overall description of what your
## functions do

## My functions create a matrix that can be inverted and stored via the
## makeCacheMatrix function and calls back the inverse of matrix so the
## calculation does not need to repeat.


## Write a short comment describing this function

## The makeCacheMatrix function sets the value and gets the value of the
## matrix.  It also sets and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) i <<- inverse
  
  getinv <- function() i
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

## This function checks for a cached calculation of the inverted matrix
## first, and then if it doesn't exist, it calculates the inverse here
## and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  
  x$setinv(i)
  
  i
  
}
