## This function creates a special "matrix" object. 
## It is a list of four functions to get and set the matrix and to get and set the cached version inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" calculates the inverse of the special "matrix" object from the function "makeCacheMatrix". 
## If the inverse already exists, it gets the inverse from the cache. Else, it calculates the inverse
## and sets the value with the "setinverse" function in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}