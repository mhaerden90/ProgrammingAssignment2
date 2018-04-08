## These functions allow one to cache the inverse of a matrix. 

## the makeCacheMatrix lists four functions to set and get the matrix, and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}

## The cacheSolve function checks whether the inverse of a matrix is already in cache, and if not calculates the inverse and stores it via the setInverse defined in MakeCacheMatrix()

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cashed data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
