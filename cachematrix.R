## Caching the Inverse of a matrix
## A "matrix" which caches its inverse when it is computed
##
## Assumes the matrix is invertible
##
## How to use: to cache the inverse of a matrix m
## - pass m to createCacheMatrix
## - call cacheSolve on the returned object to get the inverse of m
## (the inverse is computed only the first time cacheSolve is called:
## then, it is returned from the cache)
## Note that if you change m, cacheSolve will still return the inverse of the "initial m"
## (call set to reset the cache)
##
## Could actually be used to cache the result of another fucntion (not only solve(matrix))
## see makeCacheObject


## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        makeCacheObject(x, solve)
}

## Returns the inverse of a "special matrix" created with makeCacheMatrix.
## If the inverse has already been calculated,
## the inverse is retrieved from the cache.
## Note that the "..." parameters are used only in the first call
## (if you change them on next calls, the result won't be changed)
## @param x: a "special matrix" created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        x$getResult(...)
}

## Create a special object than can cache the value returned by a function fun that takes x as parameter
## If mco is the result of a call to this function:
## - call mco$getResult() to get the result (either computing it, or getting it from the cache)
## - call mco$set to change the object we're caching about, 
## - or call mco$setFun to change the function whose result is cached
makeCacheObject <- function(x, fun) {
        # init the cache
        # This is important to have the "<<-" work the way we want it to:
        # <<- looks for a var called cache in the parent environments,
        # and we want it to find it here
        cache <- NULL
        
        # to be able to check in tests that whether during last call to getResult,
        # the result has been computed, or got from the cache
        lastResultGotFromCache <- FALSE
        
        # to be used to change the object inside this "special object"
        set <- function(y) {
                x <<- y
                # reset the cache
                cache <<- NULL
        }
  
        # to be used to change the function whose result is cached
        setFun <- function(yfun) {
                fun <<- yfun
                # reset the cache
                cache <<- NULL
        }
        
        # return the object we cache about
        get <- function() x
        
        # return the result 
        getResult <- function(...) {
                if(!is.null(cache)) {
                        # cache is here, return it
                        # message("getting cached data")
                        lastResultGotFromCache <<- TRUE      
                        return(cache)
                }
                # not computed yet
                # well, compute the result of fun, store it in cache and return it
                lastResultGotFromCache <<- FALSE      
                cache <<- fun(x, ...)
        }
        
        lastResultHRetrievedFromCache <- function() lastResultGotFromCache
        
        # return a list with the functions we just created
        list(get = get,
             set = set,
             setFun = setFun, 
             lastResultHRetrievedFromCache = lastResultHRetrievedFromCache,
             getResult = getResult)
}

## a test that shows 
## how it works and that we get some expected results
testCache <- function() {
        # identity matrix
        e <- rbind(c(1, 0), c(0, 1))
        # a 2*2 matrix
        mat1 <- rbind(c(1, 3), c(3, 1))
        # crate a special matrix
        cm1 <- makeCacheMatrix(mat1)
        # compute the inverse of mat1
        inv1 <- cacheSolve(cm1)
        # inv1 %*% mat1 should be the identity
        ok <- all(e == inv1 %*% mat1)
        if (!ok) error("inv1 %*% mat1 should be the identity")
        else print("inv1 %*% mat1 is the identity matrix")
        
        fromCache <- cm1$lastResultHRetrievedFromCache()
        if (fromCache) error("Unexpected!")
        else print("Result has been computed")
        
        # second time we get the inverse, we should get the same result
        # and have the message "getting cached data"
        inv1 <- cacheSolve(cm1)
        ok <- all(e == inv1 %*% mat1)
        if (!ok) error("inv1 %*% mat1 should be the identity")
        else print("inv1 %*% mat1 is the identity matrix")
        
        fromCache <- cm1$lastResultHRetrievedFromCache()
        if (!fromCache) error("Unexpected!")
        else print("Result has been retrieved from cache")
        
        # change mat1
        print("changing mat1")
        mat1 <- rbind(c(2, 0), c(0, 2))
        # we must change it inside cm1
        cm1$set(mat1)
        # compute the inverse of mat1
        inv1 <- cacheSolve(cm1)
        # inv1 %*% mat1 should be the identity
        ok <- all(e == inv1 %*% mat1)
        if (!ok) error("inv1 %*% mat1 should be the identity")
        else print("inv1 %*% mat1 is the identity matrix")
        
        fromCache <- cm1$lastResultHRetrievedFromCache()
        if (fromCache) error("Unexpected!")
        else print("Result has been computed")
}

