## The following pair of functions compute the inverse of a given
## matrix and cache the inverse of those matrix, whose inverse has
## been previously computed.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(solve) {
                inv <<- solve
        }
        getinverse <- function() {
                inv
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by
## makeCacheMatrix function. In scenarios where the inverse has
## been already calculated (and the matrix has not changed) the 
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) { 
                message("Retrieving inverse from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
