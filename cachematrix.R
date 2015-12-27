## This pair of functions cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse
# set = set the matrix
# get = get the matrix
# setinverse = set the inverse of the matrix
# getinverse = get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(solve) m <<- solve  
  
  # get the inverse of the matrix
  getinverse <- function() m
  
  # list containing function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix ## If the inverse has already been calculated (and the matrix has not changed), ## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
