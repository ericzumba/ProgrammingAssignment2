## Returns a matrix whose inverse is cacheable
## There is no 'set' function to make x immutable
## for a given returned value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns the inverse of a cacheable matrix x
## uses a cached calculation if there is one
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
