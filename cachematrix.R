## These functions provide a way to cache the result of a possibly slow calculation,
## such as the inverse of a matrix.
## The matrix to be inverted must be inputted into the makeCacheMatrix.
## Then the object returned by makeCacheMatrix must be passed to the cacheSolve 
## function which will return the inverted matrix (either cached or computed)

## This function creates a special "matrix" object that can cache its inverse.
## Actually it's only a list of function to operate with the matrix value and 
## store its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache printing a log message.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()                         
  if(!is.null(inverse)) {                           
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                             
  inverse <- solve(data, ...)                       
  x$setinverse(inverse)                             
  inverse
}
