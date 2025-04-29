## The following functions optimize the process of calculating the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) 
        {
                inv <- NULL # Initialize the cache
                set <- function(y) 
                {
                        x <<- y
                        inv <<- NULL # Clear the cached inverse after being called
                    }
    get <- function() x # Returns the value of x
    setInverse <- function(inverse) inv <<- inverse # Save the computed inverse of a matrix
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) # Bundles together all the helper function into a single list

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
