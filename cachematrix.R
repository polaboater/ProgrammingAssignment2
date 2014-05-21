## A. Barendt (polaboater)
## May 21 2014

## These functions store an assigned invertible matrix and its mean in a cached
## state so that it is unneccessary to constantly recalculate the inverse of the
## matrix.

## makeCacheMatrix stores the matrix and a default null inverse. When acted on
## by cacheSolve, the inverse will be evaluated if it is NULL and stored.
## Based on makeVector in the Assignment 2 ReadMe

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m_inv <<- inv
    getInv <- function() m_inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}
    

## cacheSolve will take the element defined in makeCacheMatrix and solve it if
## the mean is not already cached in mCM. If the mean is present, cS will pull
## it out of the cache and return the inverted matrix.
## Based on cachemean in the Assignment 2 ReadMe

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m_inv <- x$getInv()
    if(!is.null(m_inv)) {
        message("getting cached matrix")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setInv(m_inv)
    m_inv
}
