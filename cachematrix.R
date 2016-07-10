## The first function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## The second function, cacheSolve returns the inverse of the "matrix"
## returned by makeCacheMatrix.
## The inverse will be retrieved from the cache, if it has already been
## calculated and the matrix has not changed. Otherwise it will be computed.

## This function creates a special "matrix",
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s        
}
