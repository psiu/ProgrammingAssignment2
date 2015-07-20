#################################################
## The following function set allows the creation of a matrix object with the ability to cache its inverse.  Calculating the inverse of a matrix can be a time consuming operation so providing the ability to draw upon a cache will speed up operations.
## Pre-condition:  The matrix supplied must be invertible
#################################################

## This function creates a special "matrix" object that can cache its inverse and provides the following four functions:
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse
##	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize cache to NULL
    i <- NULL
    
    ## Function definitions
    ## Set the matrix to the supplied matrix and reset the cache since the matrix has changed.
    set <- function(y) {
        x <<- y
        i <<- NULL  ## Set cache to NULL since the matrix has changed
    }
    
    ## Retrieve the stored matrix
    get <- function() x
    
    ## Set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## Retrieve the cached matrix, if no cached matrix exists, NULL will be returned
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Check if the matrix has an existing cache.  If NULL is returned, then no cache exists
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## No cache exists, calculate inverse, store it in the cache, and then return the calculated inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)    ## Cache the new calculated inverse
    return(i)
}

#############################
## Information on how to calculate an inverse of a matrix
## https://www.mathsisfun.com/algebra/matrix-inverse.html
#############################

#############################
## Examples and test results
#############################
##> source("cachematrix.R")

## Example 1, inverse of a 1:4 2x2 matrix

##> x <- matrix(1:4,2, 2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cx <- makeCacheMatrix(x)
##> cacheSolve(cx)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## Example 2, inverse of a 2x2 matrix using 4,2,7,6

##> x <- matrix(c(4,2,7,6), 2, 2)
##> x
##[,1] [,2]
##[1,]    4    7
##[2,]    2    6
##> cx <- makeCacheMatrix(x)
##> cacheSolve(cx)
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
