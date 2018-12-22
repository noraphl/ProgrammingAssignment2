## These functions cache the value of the inverse of a matrix

## This function creates an object that contains the methods for:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the the inverse matrix
## 4. get the inverse of the matrix

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


## This function finds the cached value of the marix and
## its inverse from the object created with 'makeCacheMatrix'; 
## if not found, it calculates and caches these values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
