## This is a mechanism for caching matrix inversion
## We create an object which stores internally both a matrix
## and a cached solution of inversion
## Upon request we either give the calculated value, or calculate
## and store

## makeCacheMatrix is a constructor for a matrix object with cache
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolution <- function(mean) s <<- mean
  getsolution <- function() s
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## cacheSolve calculates the inversion of the matrix given
## If the value is already stored in the cache,
## we simply give the previously calculated solution
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolution()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolution(s)
  s
}
