## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. Below are two functions that are used to create
## a special object that stores a matrix and cache's its inversion.
##
## Usage example:
## > source("cachematrix.R")
## > m <- matrix(1:4, nrow = 2, ncol = 2)
## > d <- makeCacheMatrix(m)
## > cacheSolve(d)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(d)
## getting cache data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##

## The first function, makeCacheMatrix creates a special object,
## which really a list containing a function to
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setinverse - set the value of the inverse
## 4. getinverse - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## The following function calculates the inverse of the matrix kept by special
## object created with the above function. However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the setinverse
## function. If the cache was used the appropriate message will be printed.

cacheSolve <- function(x, ...) {

    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cache data")
        ## Return a matrix that is the inverse of 'x'
        return (inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
