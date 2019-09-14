## These functions can calculate and cache the inverse of a matrix. 

## The makeCacheMatrix function is used to store a matrix, or replace with a new matrix, 
## or store the cached inverse of this matrix calculated by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
                setinv= setinv,
                getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix defined in makeCacheMatrix, cache it, 
## or retrives the cached value if there's already one. 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
