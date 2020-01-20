## Functions for caching the Inverse of a Matrix

## First, a function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i<<-inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Than, the function that computes the inverse of the sepcial matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(x)
        x$setinv(i)
        i
}
