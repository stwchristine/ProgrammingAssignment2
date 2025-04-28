## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Initialize the cache
        set <- function(y) {
        x <<- y
        inv <<- NULL # Clear the cached inverse
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


cacheSolve <- function(x, ...) {
        inv <- x$getInverse() # Check if cached inverse exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # Retrieve cached inverse
    }
    data <- x$get() # Get the matrix
    inv <- solve(data, ...) # Compute the inverse
    x$setInverse(inv) # Cache the inverse
    inv
}
