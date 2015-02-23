## The two functions below (makeCacheMatrix & cacheSolve) are used together for the 
## ultimate purpose of finding the inverse of a matrix (as well as caching said matrix's
## inverse, preventing the user from having to repeatedly compute it)

## FUNCTION #1 - makeCacheMatrix()
## The following function creates a "matrix object" that can cache its inverse;
## The function actually produces a list of 4 functions (set, get, setInverse, & 
## getInverse).

makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL  ## Important for the accompanying function (cacheSolve)
    set <- function(y) {  ## The "setter" function, setting the value of the initial (un-inverse) matrix
        x <<- y  ## Use of the "deep assignment operator"; will keep looking up the chain of parent environments to find matching name
        xInverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(invr) {
        xInverse <<- invr
    }
    getInverse <- function() {
        xInverse
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## These functions can be accessed using the $ operator (e.g. x$set, x$setInverse, etc.)
## ROLES: x$set to enter/change the given matrix; x$get to acquire the matrix from x$set;
## ROLES, cont.: x$setInverse to set the inverse of the given matrix; & x$getInverse to get that inverse matrix


## FUNCTION #2 - cacheSolve()
## This function is used in coordination with the accompanying function entitled "makeCacheMatrix"
## The object produced by makeCacheMatrix is passed into the following function ("cacheSolve") as "x"

cacheSolve <- function(x, ...) {
    matr <- x$getInverse()  ## will get the inverse of the matrix 
    if(!is.null(matr)) {  ## this if... statement will trigger if matrix has already been cached
        message("Getting cached data...")
        return(matr)  ## if already cached, the final result of the inverse matrix will be returned
    }
    data <- x$get()  ## (above condition not met), uses the "get" function & stores matrix object in "data"
    matr <- solve(data)  ## use the built-in function "solve" to calculate the inverse of a square matrix (& store result in "matr")
    x$setInverse(matr)  ## uses the "setInverse" function (from "makeCacheMatrix") to set it
    return(matr)  ## returns the inverse of the matrix originally input by user
}
