## Justin Welsh
## 2015-03-22
## Forked from ProgrammingAssignment2 @ https://github.com/rdpeng/ProgrammingAssignment2
## Original Author: Roger D. Peng
## Forked on 2015-03-22
##
##  This project will allow you to take a matrix and cache the inverse solution of the matrix for fast retrieval.   
##
##---------------------------------------------------------------------------------------------------------------
##
## makeCacheMatrix
##
## Function to create a special matrix object and cache the inverse.  
##
## Arguments: function(x = matrix())
## x is matrix
##
## Functions
##
## Set the original matrix in cache using $set function.
## Get the original matrix in cache using $get
## Set inversed matrix in cache using $setsolve
## get inversed matrix in cache using $getsolve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
      x <<- y
      s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##---------------------------------------------------------
## cacheSolve
##
## Function to solve ("inverse") a matrix and store it in cache.  
##
## Arguments: function(x)
## x is the function makeCacheMatrix for storing and retrieving matrices from cache 
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
