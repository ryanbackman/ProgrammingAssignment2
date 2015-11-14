## Functions to create the inverse of a matrix and cache the results

## makeCacheMatrix stores inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setter for matrix
    set <- function(y) {
        x <<- y
        m <<-NULL
    }
    # getter for matrix
    get <- function() {
        x
    }
    # set passed inverse to internal var
    setsolve <- function(solve) {
        m <<- solve
    }
    # return interal inverse var
    getsolve <- function() {
        m
    }
    # available functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve checks makeCacheMatrix for 
## cached inverse value and returns it or generates
## new inverse and caches in makeCacheMatrix

cacheSolve <- function(x, ...) {
    # get the cached value, if any
    m <- x$getsolve()
    # if there is a value, print and return
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If there was no cached version...
    # get the data
    data <- x$get()
    # generate the solve
    m <- solve(data, ...)
    # and cache
    x$setsolve(m)
    # return m
    m
}
