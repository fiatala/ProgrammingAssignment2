# These functions create a pseudo-matrix that allows caching and retrieval of 
#       the matrix inverse

# makeCacheMatrix creates a cacheMatrix object that allows getting and setting
#       of a matrix and its inverse
makeCacheMatrix <- function(mx = matrix()) {
        mxInverse <- NULL
        # set the matrix, and nullify any pre-existing cache
        set <- function(newMx) {
                mx <<- newMx
                mxInverse <<- NULL
        }
        get <- function() mx
        setInverse <- function(solvedInverse) mxInverse <<- solvedInverse
        getInverse <- function() mxInverse
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}

# cacheSolve returns the inverse of a matrix. Once an inverse is calculated it
#       is cached for future retrieval
cacheSolve <- function(cacheMatrix, ...) {
        mxInverse <- cacheMatrix$getInverse()
        if(!is.null(mxInverse)) {
                message("getting cached data")
                return(mxInverse)
        }
        mx <- cacheMatrix$get()
        mxInverse <- solve(mx, ...)
        cacheMatrix$setInverse(mxInverse)
        mxInverse
}