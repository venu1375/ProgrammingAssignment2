## Matrix inversion is a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## The followig functions will do the caching of the Inverse Matrix.

## This Function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) ix <<- solve
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the "matrix" returned by 
## makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}
