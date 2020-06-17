## The following Functions are used to difine a "Cached Matrix" and to 
## calculate/retrieve the inverse of the "Cached Matrix".

## "makeCacheMatrix" returns a matrix represented by a list of 4 elements.
## The "set" function sets the value of the matrix.
## The "get" function returns the value of the matrix.
## The "setinv" function sets the inverse value of the matrix.
## The "getinv" function returns the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_m) i <<- inverse_m
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## "cacheSolve" function retrieves the value of the inverse if it is returned
## in the "getinv" function of the "Cached Matrix" and the matrix has not changed.
## Else, the inverse value in the cache is updated with the new one 
## that is calculated in this function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
