## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special Matrix object that contains
## functions for getting and setting an invertible square matrix,
## and getting and setting its inverse -- all functions to be used
## by the cacheSolve function defined below.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get<- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve accepts a special matrix created using the makeCacheMatrix
## function. cacheSolve returns the inverse of a invertible square matrix
## created by the makeCachematrix function.

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
