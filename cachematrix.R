## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y) {
            x <<- y
            invMat <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) invMat <<- inverse
      getInverse <- function() invMat
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInverse()
      if (!is.null(invMat)) {
            message("getting cached data")
            return(invMat)
      }
      mat <- x$get()
      invMat <- solve(mat, ...)
      x$setInverse(invMat)
      invMat
}
