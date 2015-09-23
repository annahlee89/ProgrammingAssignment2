## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special class of matrices that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
       x <<- y
       n <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) n <<- inverse
  get_inverse <- function() n
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function gives the inverse of the makeCacheMatrix function from above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   n <- x$get_inverse()
   if (!is.null(n)) {
      message("getting cached data")
      return(n)
   }
   data <- x$get()
   n <- solve(data, ...)
   x$set_inverse(n)
   n
}
