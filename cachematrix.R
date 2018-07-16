## *Create a pair of functions that cache the inverse of a matrix*

## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { ##set the value of a matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x ##get the value of a matrix
  setinverse <- function(solve) s <- solve ##set the value of the inverse
  getinverse <- function() s ##get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}