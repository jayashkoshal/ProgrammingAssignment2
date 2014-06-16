## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# contruct a list of functions that provide an interface for the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # matrix setter function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## matrix getter function
  get <- function() x
  
  # inverse setter function
  setinverse <- function(inverse) m <<- inverse
  
  # inverse getter function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of a given matrix. If
# the inverse exist in cache then it uses the cached entry else
# computes one and caches it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the inverse using inverse getter function
  m <- x$getinverse()
  # check if retrieved from cache. Log this info
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  # get the actual matrix
  data <- x$get()
  # get inverse using the actual solve function
  m <- solve(data, ...)
  # cache the inverse for future retrieval
  x$setinverse(m)
  m
}

