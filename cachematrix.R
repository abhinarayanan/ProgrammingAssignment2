## The below functions are used to cache and calculate the inverse of input matrix.
## If the inverse of the input matrix is available on cache, it will return the cached values
## without re-calculation.  If it is not available, then it will calculate the inverse and cache it.

## This function creates a special "matrix" object that can cache its inverse which is really a list containing a function to :
## set -> set values to matrix
## get -> retrieves the matrix [source]
## setinverse -> in case if we need to override the inverse manually
## getinverse -> gets the resultant matrix [inverted]

makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL
    set <- function(y) {
        x <<- y
        mtx <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) mtx <<- solve
    getinverse <- function() mtx
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	mtx <- x$getinverse()
    if(!is.null(mtx)) {
        message("getting cached data")
        return(mtx)
    }
    data <- x$get()
    mtx <- solve(data, ...)
    x$setinverse(mtx)
    mtx
}
