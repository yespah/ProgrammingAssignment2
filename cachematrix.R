## Put comments here that give an overall description of what your
## functions do

## This function, makeCachedMatrix creates a special "matrix", which is really a list containing a function to
## set the matrix
## get the the matrix
## set the inverse matrix
## get the inverse matrix
## It is just a slightly altered version of the "makeVector" example given.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function uses solve and a special "matrix" to find the matrix inverse.
## It checks if the inverse matrix is already cache, then it uses solve
## to get and set it in the special "matrix" created by the previous function "makeCacheMatrix".
## "solve" will generate default error messages, if matrix is not square or is singular.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
