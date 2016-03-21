## This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
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
    thism <- x$getinv
    
    ## couldn't work out how to tell if the matrix was empty or not :-( 
    ## if I left it with null it always returned cached data, so never
    ## calculated the inverse, now it always calculates the inverse.
    ## am submitting with this bug as I am short of time and will try
    ## to see if I can fix it before the deadline and resubmit
    
    if(!is.null(dim(thism))) {
        message("getting cached data")
        return(thism)
    }
    data <- x$get()
    thism <- solve(data, ...)
    x$setinv(thism)
    thism
}
