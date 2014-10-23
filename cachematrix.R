## makeCacheMatrix setups up 4 functions, inlcuding a function to invert a matrix, and returns 
## the functions as a list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## Return a matrix that is the inverse of 'x'. Checks the cache m, and if m is not null, will return
## the cached data. Otherwise the function will calculate the inverse of the matrix input.
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


