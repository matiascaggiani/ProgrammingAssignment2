## Some computations in R are time-consuming. A good approach to deal with
## such computations is caching them. Matrix inversion is one example of the
## situation above mentioned. The solution is to cache the inverse of a matrix
## so next time R needs that inverse do not calculate again.



## makeCacheMatrix function creates a "matrix" object that can cache 
## its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
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

## cacheSolve function computes the inverse of the matrix created by 
## makeCacheMatrix above. If we try to comput an inverse that has been cached
## the function takes the inverse from the cache avoiding the task.


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
