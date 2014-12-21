## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a utility function that sets and gets the matrix. In addition, it also 
## sets and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function
## This function returns the inverse of a matrix. For fast access, it checks
## the inverse data in the cache and if it exists, returns it. Otherwise it computes
## the inverse, caches the data and returns.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
