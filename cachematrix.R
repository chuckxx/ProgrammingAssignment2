## Charles Smith - Programming Assignment 2: Lexical Scoping
## Write an R function that is capable of caching potentially time consuming computations.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
        matrixInverse <- NULL
        y <- NULL
        # set the matrix and reset the cached inverse
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        # get the matrix
        get <- function() x
        # calculate the inverse of the matrix using the solve function
        setInverse <- function(solve) matrixInverse <<- solve
        # get the inverse 
        getInverse <- function() matrixInverse
        # returns a special vector (i.e. list of functions)    
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
  
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of x
        matrixInverse <- x$getInverse()
        data <- x$get()
        ## first see if the inverse is cached and the matrix is unchanged
        if(!is.null(matrixInverse) && identical(matrixInverse,solve(data,...))) {
                message("inverse is cached and matrix hasn't changed")
                return(matrixInverse)
        }
        message("inverse is not cached; solving inverse")
        matrixInverse <- solve(data, ...)
        x$setInverse(matrixInverse)
        matrixInverse
}
