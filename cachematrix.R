## Function that caches the inverse of a matrix
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (...). 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of matrix <- makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    #if we have calculted the inverse before, return it
    message("getting cached data")
    return(inv)
  }
  # calc and store the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
