## The following set of functions allows users to cache the inverse of a matrix.
## The cached value could be used in other calculations to speed up the perform
## ance.

## makeCacheMatrix is used to initialize the matrix, store this matrix and it's 
## inverse and provides interfaces to reinitialize or output stored objects.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve is a higher-order function intended to use makeCacheMatrix as
## an argument. cacheSolve gets the matrix stored in makeCacheMatrix calculates
## it's inverse and cache it via makeCacheMatrix interface.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting data from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
