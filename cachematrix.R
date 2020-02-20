## This file contains two functions which work together to allow a user
## to calculate the inverse of an inversible matrix and cache that inverse 
## for future use


## makeCacheMatrix creates a special matrix object with getters and setters to the
## function itself as well as the inverse of the original user-provided matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will accept the object created by makeCacheMatrix and will
## calculate the inverse of that special matrix object first by checking
## if it has already been saved in the cache and then proceeding to calculate
## the inverse only if it was not found

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i) && !is.na(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
