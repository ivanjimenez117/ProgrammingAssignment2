## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, get the value of the inverse


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


## This function calculates the inverse of the special "matrix" created with
## the above function. First checks to see if the inverse has already been calculated

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
