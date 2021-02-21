## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a new matrix based R object which contains a matrix and its inverse.
## Following operations can be performed on this object.
## set -> assign a matrix.
## get -> Read matrix.  
## setinvrs -> set the value of inverse of the matrix.
## getinvrs -> get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(y) invrs <<- y
  getinvrs <- function() invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
  
}


## Write a short comment describing this function
## This function is used for calculating the inverse of special matrix 
## created by makeCacheMatrix.
## If the inverse is not yet calculated (if it is NULL) ,
## it calculates the inverse and saves it in the special matrix.
## Otherwise it returns the already calculated inverse,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinvrs(invrs)
  invrs
  
}
