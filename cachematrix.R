## This set of functions allows to calculate an inverse of a matrix
## and cache the results speed up following calls. 
##
## Usage:
## > m <- matrix(rnorm(9), 3, 3)
## > cache <- makeCacheMatrix(m)
## > cacheSolve(cache)

## Create a cache to to calculate a matrix inverse.
## x - an invertible matrix
## Returns: a cache with the following operations
## - set - set the value of the input matrix
## - get - get the value of the input matrix
## - setinverse - set the cached matrix inverse
## - getinverse - get the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   
   setinverse <- function (inverse) m <<- inverse
   getinverse <- function () m
   
   list(set = set, get = get, 
        setinverse = setinverse, getinverse = getinverse)
}


## Calculate a matrix inverse using the given cache "x"
## x - a cache created by makeCacheMatrix
## Returns: matrix inverse

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if (!is.null(inverse)) {
       message("getting cached inverse")
       return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setinverse(inverse)
    inverse
}
