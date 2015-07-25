## The first function, makeCacheMatrix, creates a cache for an invertible matrix and its inverse.
## The second function, cacheSolve, acts on the output of makeCacheMatrix and returns the inverted
## matrix. If cacheSolve is run again on the same argument, it retrieves the inverse from the cache. 

## The makeCacheMatrix function initializes an empty cache (m) for the inverted matrix. it also creates 
## a group of functions that can be called to manipulate and retrieve a matrix.
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

## The cacheSolve function performs the matrix inversion calculation. It takes as its argument
## the output of makeCacheMatrix. If the function has already been run on this same argument, 
## it retrieves the result from the cache and prints a message to that effect.
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
     ## Return a matrix that is the inverse of 'x'
}