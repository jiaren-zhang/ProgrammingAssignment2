## Caching the Inverse of a Matrix
## This makeCacheMatrix function is used to create a special object that stores a matrix 
## and caches its inverse.

makeCacheMatrix <- function(a = matrix()) {
  b <- NULL
  set <- function(c) {
    a <<- c
    b <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse)  b <<- inverse
  getinverse <- function() b
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Here the function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated (
## and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(a, ...) {
  b  <- a$getinverse()
  if (!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- a$get()
  b <- solve(data, ...)
  a$setinverse(b)
  b
}
