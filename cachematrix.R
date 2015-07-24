## These two functions create a cached version of the inverse
## of a matrix so that it does not have to be computed repeatedly.

## Creates a list containing a function that:
## Sets and gets the value of a matrix
## Sets and gets the inverse value of a matrix
makeCacheMatrix <- function(x = matrix()) {
    # Set value of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get value of the matrix
    get <- function() x
    
    # Set value of the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Get value of the inverse of the matrix
    getInverse <- function() inv
    
    # Return list
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns the inverse of a matrix by first checking to
## see if the inverse is cached and, if not, performs
## the calculation.
cacheSolve <- function(x, ...) {
    # Check to see if inverse of matrix is cached
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        # If so, get the cached version and return that
        message("Getting cached data...")
        return(inverse)
    }
    
    # If not, get the matrix and inverse it
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    
    # Return inversed matrix
    inverse
}
