makeCacheMatrix <- function(x = matrix()) {
  inversevalue <- NULL
  set <- function(y) {
    x <<- y
    inversevalue <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversevalue <<- inverse
  getinverse <- function() inversevalue
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  inversevalue <- x$getinverse()
  if(!is.null(inversevalue)) {
    message("getting cached data")
    return(inversevalue)
  }
  data <- x$get()
  inversevalue <- solve(data, ...)
  x$setinverse(inversevalue)
  inversevalue
}