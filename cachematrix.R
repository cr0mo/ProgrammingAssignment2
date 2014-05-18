## The functions store a matrix and calculates its inverse.

## This function creates a list that has functions to set and get values of a 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invx <<- inv
    getInverse <- function() invx
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function calculates the inverse of the matrix that was stored with the
## previous function, but first it checks if the inverse was calculated before
## and cached. In that case it uses the cached inverse.

cacheSolve <- function(x, ...) {
    invx <- x$getInverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data)
    x$setInverse(invx)
    invx
}

