## These functions can calculate and cache the inverse of a matrix. 

## The makeCacheMatrix function is used to store the matrix, replace it a new matrix, or store its cached inverse.

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


## The cacheSolve function calculates the inverse of the matrix defined in makeCacheMatrix, or retrives cached value. 

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
