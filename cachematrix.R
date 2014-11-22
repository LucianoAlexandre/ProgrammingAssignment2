## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Method Set Matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method Get Matrix
  get <- function() {
    m
  }
  
  ## Method Set the Inverse Matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##Method Get Inverse Matrix
  getInverse <- function() {
    i
  }
  
  
  ##Return list of Methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function

## Compute Inverse Matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated,
## then the retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}
