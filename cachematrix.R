## Function to create a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Functiion to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if (!is.null(i)) {
       message("getting cached data")
       return(i)
     }
     mat <- x$get()
     i <- solve(mat, ...)
     x$setinv(i)
     i
}
