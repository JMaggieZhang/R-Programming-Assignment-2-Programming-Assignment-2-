## The makeCacheMatrix and cacheSolve create to store a matrix and cache the inverse of that matrix. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## It contains four functions to 
## 1. set -> set the value of the matrix
## 2. get -> the value of the matrix
## 3. cacheInverse ->  cache the inverse of the matrix
## 4. getInverse ->  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 M <- NULL
 
 ## set is a function that changes the matrix stored in the main function
 set <- function(y) {
         x <<- y
         M << - NULL 
    
  get <- function () x
  cacheInverse <- function(inverse)  M <<- inverse   ##"inverse" here is to store a value but not to compute
  getInverse <- function() M
  
  ## The following line stores the four functions
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## cacheSolve is a function to compute the interse of the special "matrix" returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
     M <- x$getInverse ()
     if(!is.null(M)) {
          message("getting the cached matrix")
          return (M)
          }
      data <- x$get()
      M <- solve(data, ...)
      x$cacheInverse(M)
      M
}
