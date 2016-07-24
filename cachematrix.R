## This function creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminverse <- function(compute) m <<- minverse
  getminverse <- function() m
  list(set = set, get = get,
       setminverse = setminverse,
       getminverse = getminverse)
}


## This function computes the inverse of the special
## "matrix" returned by  makeCacheMatrix  above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- compute(data, ...)
  x$setminverse(m)
  m
}
