## Caching the Inverse of a Matrix

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