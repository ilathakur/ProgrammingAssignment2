## Overall Description Of the Functions - The following computation consists of two functions 1) makeCacheMatrix and2) cacheSolve. The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by the first function. It also checks whether the inverse has already been calculated (and the matrix has not changed). If this is true, then the second function retrieve the inverse from the cache.

## makeCacheMatrix is a function that creates a square matrix and caches the inverse of this matrix. 

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) cache <<- solve
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##The cacheSolve function calculates the inverse

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setinverse(cache)
  return(cache)
}
  
  