## Put comments here that give an overall description of what your
## functions do

## this function will take a matrix and return a list of useful functions:
## get() to extract/read/show the matrix
## set() directly to change the underlying matrix
## getinv() to get the inverse of the matrix
## setinv() to store the value of the matrix inverse for future use
## Note: that the function set() resets the cached mean every time its called with 
## a new vector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## this is the function that will actually solve the inversion using the previous function if needed
## starts by checking is there is already an inverse (from a call to $getinv) and 
## if not found then proceeds to calulate the inverse 
## after calylating the inverse the function x$setinv() is called that will store it
## for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
