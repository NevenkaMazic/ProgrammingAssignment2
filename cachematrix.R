
## This function creates a matrix object, which is a list containing four parts

makeCacheMatrix <- function(x = matrix()) {

    # defining the cache matinv as NULL
    matinv <- NULL
    
    set <- function(y) {
        
        # assigning the input matrix y to variable x in the parent environment:
        x <<- y 
        
        # again initializing matinv to null:
        matinv <<- NULL
        
    }
    
    get <- function() x  # takes matrix x
    
    setinv <- function(inverse) matinv <<- inverse   # sets the cashe matinv equal 
    # to the inverse of the matrix x 
    
    getinv <- function() matinv   # gives the cached inverse of x
    
    # returns a list of all four objects:
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function calculates the inverse of the above created matrix. 
## It only does this if the inverse has not yet been calculated (and cached). 
## If it was calculated, the cacheSolve will return the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    # Returns a cached matrix if there is one:
    matinv <- x$getinv()
    if(!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    
    # if there is nothing in the cache, it calculates the inverse of the matrix:
    data <- x$get()
    matinv <- solve(data)
    x$setinv(matinv)
    matinv
    
}
