# Test:
# m <-matrix(c(4,2,7,6),nrow=2,ncol=2)
# cacheSolve(makeCacheMatrix(m))

# The following functions are intended to improve the performance in a computation
# By using a cache instead of recalculating everything again.

# Thisfunction creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) m <<- solved
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# This function calculates the inverse matrix using the cache.
# if the inverse is already calculated the cache retrieves the inverse from there.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
