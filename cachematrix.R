## Two functions are provided to create a special matrix object
## the special object can cache a stored matrix and its invers
## In case a new matrix is stored the inverse is recalculated
## when it is requested, otherwise the cached invers is retrived

## Creates a special matrix object and provides interface for setting
## getting a matrix and settign and getting it's invers

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks wheter the invers of stored matrix is already calculated and
## in that case retrieves and returns the cached invers
## Otherwise inverse is caclulated and then returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)){
    message("getting cached data")
    return (invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
