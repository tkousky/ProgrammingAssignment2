## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly

## note: for this code, we assume that the matrix supplied is 
## always invertible.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## 'x' is a special cacheable matrix created using 
    ## makeCacheMatrix (we will assume the underlying matrix 
    ## contained in x is invertible - For example, if X
    ## is a square invertible matrix, then solve(X) returns its 
    ## inverse and we will assume that solve(X) will work)
    
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if (! is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
