## This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function checks to see if the matrix already exists if
## so it will return the matrix, otherwise it will compute and
## return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    thism <- x$getinv()
    
    
    if(!is.na(thism)) {
        message("getting cached data")
        return(thism)
    }
    data <- x$get()
    thism <- solve(data)
    x$setinv(thism)
    thism
}
