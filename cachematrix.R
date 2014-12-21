#Your assignment is to write a pair of functions that cache the inverse of a matrix.
#
#Write the following functions:
#
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, should include inner functions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#
##For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    invSet <- NULL
    set <- function(inSetMatrix) {
        x <<- inSetMatrix
        invSet <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invSet <<- inverse
    getinverse <- function() invSet
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve: This function include:
#            1. Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#            2  If the inverse has already been #calculated (and the matrix has not changed), then the cachesolve should 
#               retrieve the inverse from the cache.
#            3. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a 
#               square invertible matrix,#then solve(X) returns its inverse.
#

cacheSolve <- function(x, ...) {
    invSet <- x$getinverse()
    if(!is.null(invSet)) {
        message("getting cached inverse data")
        return(invSet)
    }
    data <- x$get()
    invSet <- solve(data)
    x$setinverse(invSet)
    invSet
}
