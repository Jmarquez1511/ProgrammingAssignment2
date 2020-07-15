## makeCacheMatrix creates a list containing a set of four functions. 
## The function will take in a matrix and uses lexical scoping with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set is a function that takes in y, because of lexical scoping, y will be the matrix x and will be retrieved from 
  ## makeCacheMatrix
  ## Set will also set m to null, m will be used later in cacheSolve to store the cache inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get passes the value of the matrix x
  get <- function() x
  ## setsolve completes by ussing <<- for to assign m to the function solve from the parent environment
  setsolve <- function(solve) m <<- solve
  ## getsolve defines the getter for the inverse m. Once again we use lexical scoping
  getsolve <- function() m
  ## This last section of code assigns each of the functions as an element within list()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  ## The function returns a fullt formed object of the type makeCacheMatrix() to be used by downstream R code
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Sets the matrix to be inverted from makeCacheMatrix$getsolve
  m <- x$getsolve()
  ## The next if statement passes the current cached mean or calculates a new one in the case that we send a new matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Whenever we pass a new value in makeCacheMatrix$set then 
  ## assgin the new matrix to data
  data <- x$get()
  ## Calculate de inverse
  m <- solve(data, ...)
  ## Calculate the inverse
  x$setsolve(m)
  ## return the inverse
  m
}
