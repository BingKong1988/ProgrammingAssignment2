## makecachematrix takes a matrix and store the inverse
## cachesolve checks if the inverse is stored in the cache, if not, the inverse will be created


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
