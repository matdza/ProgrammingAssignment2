# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of inverse of the matrix
# d) get the value of inverse of the matrix

makeCacheMatrix <- function(x1 = matrix()) {
  inv_1 <- NULL
  set <- function(y1) {
    x1 <<- y1
    inv_1 <<- NULL
  }
  get <- function() x1
  setinverse <- function(inverse) inv_1 <<- inverse
  getinverse <- function() inv_1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been calculated If so, it gets the result and skips the
# computation. If not, it calculates the inverse, sets the value in the cache using
# setinverse function.

cacheSolve <- function(x1, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_1 <- x1$getinverse()
  if(!is.null(inv_1)) {
    message("Loading cached data.")
    return(inv_1)
  }
  data <- x1$get()
  inv_1 <- solve(data)
  x1$setinverse(inv_1)
  inv_1
}
