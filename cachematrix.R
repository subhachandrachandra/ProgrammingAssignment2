## The following functions can be used to compute the inverse of a matrix. They
## implement an optimization where the results of the computation are cached and
## looked up when needed rather then being recomputed.

## This function creates a special "matrix" object that can cache its inverse. It
## really is a list containing functions to 
## * set the value of a matrix
## * get the value of a matrix
## * set the value of the inverse matrix
## * get the value of the inverse matrix

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


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed, then cacheSolve retrieves the inverse from the
## cache.

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
