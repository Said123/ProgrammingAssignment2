## A set of functions for computing and caching the inverse of a matrix
## or retrieving the cached inverse if it was computed before

## makeCacheMatrix creates a "special matrix"-object. It returns a list of functions
## for setting/getting the matrix and setting/getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    
    #set function for the matrix (When the matrix value is changed via set,
    #the inverse is reset to NULL)
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    #get function for the matrix
    get <- function() x
    #function that sets the inverse to a given parameter
    setInv <- function(InvResult) invMat <<- InvResult
    #function for getting the inverse if it is cached (returns NULL otherwise)
    getInv <- function() invMat
    
    #return value: a list, containing the subfunctions above
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve calculates the inverse of a special matrix created with
## makeCacheMatrix. If the inverse was already computed before, it skips
## the calculation and returns the cached value. Otherwise the inverse is
## computed and stored with the setInv function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInv()
    if(!is.null(invMat)) {
        message("getting cached inverse")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
}
