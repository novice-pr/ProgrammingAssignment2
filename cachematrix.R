## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        ## function to create a special matrix object that can cache its inverse
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## function retrieves inverse from cache or computes if required
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
