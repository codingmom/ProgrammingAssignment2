## These two functions will create an object that stores a matrix and caches it's inverse.

## makeCacheMatrix creates a list that sets the value of a matrix, gets the value of a matrix, sets the value of its inverse, and gets the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve computes the inverse of the matrix created with the makeCacheMatrix function. If the inverse was previously calculated, the inverse computation is skipped and the inverse matrix value is retrieved from the cache. Otherwise, the inverse is calculated and its value set and cached.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
