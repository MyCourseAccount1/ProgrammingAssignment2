

## makeCacheMatrix creates a list containing functions to set and get matrix values, 
## set and get the inverse of the matrix values.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, 
           get=get, 
           setinverse=setinverse, 
           getinverse=getinverse)
}

## functions checks whether inverse is computed already, 
## if not the computation of inverse is done.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}

