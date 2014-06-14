## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    out <- tryCatch(
    {
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)    
        x$setinv(i)
        i
    },
    error=function(cond) {
        message(paste("Input matrix is singular (or not invertible)...", url))
        #message("Here's the original error message:")
        #message(cond)
        return(NA)
    }
    )
}
