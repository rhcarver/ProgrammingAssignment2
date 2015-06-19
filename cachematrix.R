## Assignment 2 R Carver
## functions makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a "special" matrix object that can cache
##    its inverse
## cacheSolve computes the inverse of the prior special matrix.
##    if the inverse has already be computed and the matrix is unchanged
##    then the function simply retrieves the inverse from the cache

## User calls this function by passing an invertible matrix to the function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)      
      
}

## cacheSolve function calculates the inverse of the matrix created
##    by makeCacheMatrix. If original matrix is unchanged, it used the
##    cached inverse; otherwise, it computes the inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("now getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
