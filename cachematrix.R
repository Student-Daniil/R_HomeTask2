makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)

  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


