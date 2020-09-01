## Creates special object "matrix", caches the inverse of "matrix"

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y){
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) M <<- inverse
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse, if the inverse has already been computed, retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
      M <- x$getinverse()
      if(!is.null(M)) {
        message("getting cached inverse")
        return(M)
      }
      data <- x$get()
      M <- solve(data, ...)
      x$setinverse(M)
      M
}
