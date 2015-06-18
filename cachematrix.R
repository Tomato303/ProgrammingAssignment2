## set allows you to set a new matrix in place of original
## get allows you to see the matrix which is in the function
## setinv allows you to set the inv to cache particular inverse
## getinv allows you to retrieve the cached matrix inverse

## This funtion contains all four of the above functions within it
## They can all be called independently

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the matrix if one is not already saved
## and the matrix has nto changed

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