## The following functions compute the inverse of a matrix, store the inverse
## in a chache so that if the inverse of the same matrix needs to be computed,
## the calculation does not have to be repeated

## The function makeChacheMatrix sets the value of the matrix and calculates
## the inverse of the matrix

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


## The function cacheSolve checks for a cached matrix, and then calculates
## the inverse if it does not exist and returns the cached value if it does
## exist

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
}

