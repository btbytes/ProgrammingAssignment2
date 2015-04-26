#  Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## computing it repeatedly. Here are a pair of functions that cache
## the inverse of a matrix.


## makeCacheMatrix - create a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

    matInverse <- NULL
    set <- function (y) {
        x <<- y
        matInverse <<- NULL
    }

    get <- function() x
    setinverse <- function (mi) matInverse <<- mi
    getinverse <- function () matInverse

    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse
          )
}



## cacheSolve - compute the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    matInverse <- x$getinverse()
    if (!is.null(matInverse)){
        message("getting cached data")
        return(matInverse)
    }

    data <- x$get()
    matInverse <- solve(data, ...)
    x$setinverse(matInverse)
    matInverse
}
