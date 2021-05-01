## Function to create a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv_matrix) inverse <<- inv_matrix
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Functiion to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$set_inverse(inverse)
  inverse    
}
