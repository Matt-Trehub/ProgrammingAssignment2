## Put comments here that give an overall description of what your
## functions do
# These functions are used to create a special object that stores
# a numeric vector and cache's its mean.

## Write a short comment describing this function
# calculates the mean of the special "vector" 
# It first checks to see if the mean has already been 
# calculated. If so, it gets the mean from the cache and 
# skips the computation. 
# Otherwise, it calculates the mean of the data and sets 
# the value of the mean in the cache via the setmean function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
# This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above. If the 
# inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

