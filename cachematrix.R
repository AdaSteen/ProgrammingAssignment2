## These functions create a matrix that is stored in cache where the inverse of a square matrix is saved.

## Make a matrix to be stored in cache

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) m <<- inverse_matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Use the original square matrix to determine the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    originalmatrixdata <- x$get()
    m <- solve(originalmatrixdata, ...)
    x$setinverse(m)
    m
}
