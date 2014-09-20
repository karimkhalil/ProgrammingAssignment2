## Finds and cashes the inverse of a given square matrix. Assumes the given
## matrix is non-singular.

## First, we create a speacial object that can set and get the matrix and its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(xinvIn) xinv <<- xinvIn
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The following function computes and cashes the inverse of the matrix stored 
## in the object x (which is the result of the above function) if it is not 
## already cashed and retrieve the cashed values otherwise.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data)
    x$setinv(xinv)
    xinv
}