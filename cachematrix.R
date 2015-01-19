## Caching the Inverse of a matrix
## A "matrix" which caches its inverse when it is computed
##
## Assumes the matrix is invertible
##
## How to use:
## 	- pass a matrix to createCacheMatrix
## 	- call cacheSolve on the returned object to get the inverse of the matrix


## Creates a "matrix" which caches its inverse when it is computed
## @param x: a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # the cache
	# set the matrix
        set <- function(y) {
                x <<- y
		# reset the cache
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
	# return a list with the functions we just created
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of a "matrix" created with makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## the inverse is retrieved from the cache.
## @param x: created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
