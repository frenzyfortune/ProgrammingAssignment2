## Functions to cache the inverse of a matrix 

## makeCacheMatrix creates a special matrix with an 'inverse' attribute that can be 
## set or read.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve first checks for a cached inverse and returns it, if found.
## If not found, it calculates the inverse and assigns it to the special matrix

cacheSolve <- function(x) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
