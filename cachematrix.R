# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
  x <<- y
  f <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) f <<- solve
  getinverse <- function() f
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  f <- x$getinverse()
  if(!is.null(f)) {
    message("Getting Cached Data ... ")    # optional
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setinverse(f)
  f
}
