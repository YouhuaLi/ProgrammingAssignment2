## two functions that can inverse a matrix object and cache the result. 
## can be tested in this command in R shell:
## cacheSolve(makeCacheMatrix(rbind(c(3L,4L),c(5,6))))

## special function to construct a list object can be used in cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setsolve <- function(solve)
        m <<- solve
    getsolve <- function()
        m
    list(
        set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
    
}


## function that can inverse a matrix. 
## If we compute its inverse before, it will return from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
    
}
