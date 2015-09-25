## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function stores a matrix and a list of functions to act on that matrix
## but it does not inverse the matrix or store the inverse until the
## setinverse function is called -- WPF

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function takes the list created in the first function and checks if 
## an inverse has been stored yet. If so, it recalls that inverse matrix.
## Otherwise it computes that inverse matrix and calls the setinverse function
## to store it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse of matrix")
                return (m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
