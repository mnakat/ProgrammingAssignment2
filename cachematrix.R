# Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    set <- function(y) {
        x <<- y
        inversem <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) inversem <<- inversematrix
    getinverse <- function() inversem
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix function.
# If the inverse has already been calculated.
cacheSolve <- function(x, ...) {
    inversem <- x$getinverse()
    if(!is.null(inversem)) {
        message("getting cached data")
        return(inversem)
    }
    data <- x$get()
    inversem <- solve(data, ...)
    x$setinverse(inversem)
    inversem
}
