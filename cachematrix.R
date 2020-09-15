## The functions below aim to reduce throughput of the processor by allowing cached copies of matrix inversion. This is especially useful if there are repeated requests to gather the inverse of the same matrix.

## This function caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inversematrix) m <<- inversematrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function checks if there is a cached copy of the inverse matrix. If so, it will retrieve the cached copy of the inverse of the matrix. If not, it will calculate the inverse of this matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
