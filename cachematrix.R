
#we have two functions. First creates an object, that have matrix properties. 
#Second helps us to minimize a memory usage checking out whether the inverse matrix was calculated before or no

#makeCacheMatrix helps us to convert a simple matrix into an object with some aditional properties
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

#cacheSolve indicates whether we have a value of inverse matrix or no. 
#If we have it, the function ignores calculations and takes the information about reverse matrix from cache
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


