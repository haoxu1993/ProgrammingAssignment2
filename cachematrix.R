## A pair of functions that cache the Inverse of a Matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" 
##    object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## 'inv' is the inverse matrix, either the calculated and cached value, or set to default NULL for new object
        inv <- NULL
        ## set() sets/stores the values of a matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## get() gets/extracts the values of a matrix
        get <- function() x
        ## setinverse() sets/stores the values of the inverse of a matrix
        setinv <- function(inverse) inv <<- inverse 
        ## getinverse() gets/extracts the values of the inverse of the matrix
        getinv <- function() inv
        ## Return a list of methods/functions
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}


## 2. cacheSolve: This function computes the inverse of the 
##    special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix
##    has not changed), then the cachesolve should retrieve the 
##    inverse from the cache.

## Argument x requires an object created with makeCacheMatrix().
cacheSolve <- function(x, ...) {
        ## First it checks whether inverse value (inv) has already been cached
        ## if so, it skips the calculation and returns the inverse matrix (inv) with a message
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## if im value is NULL, it calculates the inverse matrix of the data and caches it via the setinverse() function
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv 
}
