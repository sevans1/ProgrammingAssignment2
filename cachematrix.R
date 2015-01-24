## The first function, makeCacheMatrix, can create a matrix object that will cache it's counterpart inverse.
## The second function, cacheSolve, will compute the inverse of a matrix object created by makeCacheMatrix and
## retrieve the inverse matrix from the cache if it is not changed, and will cache the inverse itself if the matrix
## is undefined.

## makeCacheMatrix creates special matrix objects that will cache their own inverse matrices.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve computes the inverse of a matrix object and retrieves it from the cache if it has already
## been calculated. Otherwise cacheSolve will cache the inverse matrix itself.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  return(m)
}
