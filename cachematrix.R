## Program to Cache an Inverse Matrix
## makeCacheMatrix : Creates a special "matrix" object that can cache its inverse.
## cacheSolve : Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function cache an inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  inv <<- solve(x)           ## Finding the inverse of the matrix
  
  setinv <- function(solve) inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This computes the inverse of a matrix returned by make CacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {      ## If the inverse of the matrix is not null this will pull the matrix from cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}