## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
   if (!is.null(inv)) {
      message("getting cached data")
      return(n)
   }
   data <- x$get()
   n <- mean(data, ...)
   x$set_inverse(n)
   n
}
