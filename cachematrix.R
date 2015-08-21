## These functions calculate the inverse of a matrix and cache that value
## If the value is already cached, then it is retrieved instead of being calculated from scratch

## Create a special vector which contains functions to do the following
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Calculates the inverse of the matrix created with the above function
## First, checks to see if the inverse has already been calculated
## If it has, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse and set the value via setinverse()

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
