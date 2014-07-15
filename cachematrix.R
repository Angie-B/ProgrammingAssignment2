## Creates a cached matrix and stores it's inverse for later access

## Makes a matrix that is cached in the parent environment and
## also has the ability to store the inverse in the parent environment
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     ## change the matrix, and reset the inverse
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     
     ## returns the 'matrix' which is actually a list of functions
     ## that store the data 
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Using a 'matrix' created by makeCacheMatrix, calculates, caches, and
## returns the inverse. If the inverse is already stored, just returns
## the cached value.
cacheSolve <- function(x, ...) {
     ## gets the stored inverse
     inv <- x$getinverse()
     
     ## if the inverse is not NULL, returns the cached data
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     ## if inv=NULL then calculates the inverse, stores it, and returns it.
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}
