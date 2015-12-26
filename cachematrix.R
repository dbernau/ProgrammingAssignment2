## Put comments here that give an overall description of what your
## functions do

## Logic for definition and initialization of the CacheMatrix Object.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Logic for checking if the inverted Matrix has already been calculated for a given CacheMatrix Object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (exists('x$getinverse')&&exists('x$setinverse')){
            inv <- x$getinverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            # Providing the inverse for a square matrix
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
        } else {
        	stop('x is not of type CacheMatrix')
        }
    }
