## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## x is a square invertible matrix
  ## Returns a list containing functions to set and get the matrix and set and get the inverse 
  ## This list is used as the input for casheSolve()
  
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special
## matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()
  ## Returns inverse of the original matrix
  
  inv <- x$getinv()
  
  ## If the inverse has already been calculated then:
  
  if(!is.null(inv)) {
    
  ## Get it from the cashe and skip the computation
    
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, calculate the inverse:
  
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  ## Sets the value of the inverse in cache through the setinv function
  
  x$setinv(inv)
  
  return(inv)
}

  ## An example

  m <- matrix(rnorm(25),5,5)
  
  ## Print value of m
  
  m
  
  ## Returns a list containing functions to set and get the matrix and set and get the inverse
  ## This list is input to cacheSolve()
  
  g <- makeCacheMatrix(m)
  
  ## Returns the inverse
  
  cacheSolve(g)
  
  
  
