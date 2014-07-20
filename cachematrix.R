## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve are functions that allow a
## prior matrix inverse calculation to be retrieved from cache

## Write a short comment describing this function
## makeCacheMatrix creates a list that effectively defines a function to
## compute a matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
 ##Set the value of the new (cacheable) vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
 ##Get the value of the vector
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
 #Set and Get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
##Determines if inverse exists. If not then computes it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ##If Inverse exists then retrieve it and return
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If Inverse does not exist then retrieve matrix and compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}