## The following functions help to cache a matrix and its inverse
## in order to avoid recalculations.
## Note that the matrix is supposed to be revertible.

## makeCacheMatrix creates a special object, which is really a list
## containing a function to:
## set - set the value of the the matrix and cache's it
## get - get the matrix
## setinv - set the inverse of the matrix and cache's it
## getinv - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix either from the cache
## if it was previously calculated or it calculates the inverse,
## stores it in the cache and returns it.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
