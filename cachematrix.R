## The functions below cache the inverse of a matrix. The pair of functions
## cache and compute the inverse of a square invertible matrix.

## The function makeCacheMatrix is a function that creates a special matrix that
## can cache its inverse by getting and setting the values of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
    set <- function (y){
        x <<- y
        i <<- NULL
      }
    get <- function () x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. However, it 1st checks if the inverse has already been
## obtained, in which case it gets the inverse and skips the computation. If
## this is not the case then it calculates the inverse using the solve function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
  
    if(!is.null(i)){
    
      message("getting cached data")
      return(i)
  }
  
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i          ## Returns a matrix that is the inverse of 'x'
}
