## The following function creates a unique "matrix" object that can have its inverse.
makeCacheMatrix <- function(x = matrix()) {

    val_inverse <- NULL 
    set <- function(y) {
    x <<- y
    abc <<- NULL
  }
  get <- function() x
  set_val_inverse <- function(i) val_inverse <<- i
  get_val_inverse <- function() val_inverse
  list(set = set, get = get, set_val_inverse = set_val_inverse, get_val_inverse = get_val_inverse)
}


## This function computes the inverse of the special "matrix" given by makeCacheMatrix.
## If the inverse has already been calculated, the cacheSolve should get it from the cache (and the matrix has not changed).

cacheSolve <- function(x, ...) {
      
  val_inverse <- x$get_val_inverse()
  if (!is.null(val_inverse)) {
    message("Getting cached data")
    return(val_inverse)
  }
  arraym <- x$get()
  val_inverse <- solve(arraym, ...)
  x$set_val_inverse(val_inverse)
  val_inverse
}

x <-makeCacheMatrix(matrix(c(89, 85, 6, 12, 4, 7, 65, 1, 6)),ncol=3,nrow=3))
cacheSolve(x)
x$get()
x$get_val_inverse()
