## Matrices generally take a lot of computational power, so to reduce the
## load we are writing a function to cache the inverse of a matrix rather
## than loop/compute it repeatedly.

## makeCacheMatrix creates a matrix object that can cache its inverse that can
## be called upon at a later time.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes for the inverse of makeCacheMatrix or retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
