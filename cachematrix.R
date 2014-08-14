##
## Two functions that makes it easy to implement a chaching scheeme 
## in your project
##
## Usage:
## m <- matrix(4:9, 2,2)
## mc <- makeCacheMatrix(m)
## cacheSolve(mc)
##

## Wraps a matrix in an chaching environment containing
## getters and setters.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Inverts a matrix. 
## Checks the chache before inverting the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
