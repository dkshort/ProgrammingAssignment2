## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix
## This function takes a matrix and returns a list.  Each element in the
## list is a function that will respectively set and get the original matrix
## and set and get a cached inverse of that matrix, should it have been set.

makeCacheMatrix <- function(cachedMatrix = matrix()) {

    cachedInverse <- NULL
    
    set <- function(newMatrix) {
        storedMatrix <<- newMatrix
        cachedInverse <<- NULL
    }
    
    get <- function() {
        cachedMatrix
    }
    
    setInverse <- function(inverse) {
        cachedInverse <<- inverse
    }
    
    getInverse <- function() {
        cachedInverse
    }
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)   
}


## function cacheSolve
## This function takes a list of the type created by the makeCacheMatrix function,
## and returns the inverse of that matrix.  It tests to see if the inverse has 
## already been cached.  If so it returns the cached result, if not it runs the
## solve function to calculate the inverse, and caches the result for future calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getInverse() ## get cached inverse

    if(is.null(inverse)) {
        ## No cached inverse - have to recalculate
        cachedMatrix <- x$get()
        inverse <- solve(cachedMatrix, ...)
        x$setInverse(inverse)
    }
    
    inverse

}
