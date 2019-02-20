## The following provide a method to cache an inverse of a matrix

## This method creates a wrapper Vector that wraps a matrix, and holds its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This method creates an inverse matrix.  It reads the inverse from the cache if it exists
## and it stores the response to cache on first calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}