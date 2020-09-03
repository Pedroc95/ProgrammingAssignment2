## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that
## has data and properties which can be cached
## Setters and getters for the matrix are implemented

makeCacheMatrix <- function(x = matrix()) {
    ## inv holds the inverse of the matrix
    inv <- NULL
    
    ## set function for setting the data of the matrix
    set <- function(y) {
        ## x is the matrix
        x <<- y
        inv <<- NULL
    }
    
    ## get function for retrieving the set data
    ## of the matrix
    get <- function() x
    
    ## set function for inv
    set_inv <- function(prov_inv) inv <<- prov_inv
    
    ## get function for inv
    get_inv <-function() inv
    
    ## Return functionality
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

## Write a short comment describing this function
## This function will return the inverse of a
## matrix created with the makeCacheMatrix() function.
## If the inverse of the matrix was previously calculated
## and cached, the function will return the stored inverse.
## Otherwise, it will calculate the inverse of the matrix
## and store the result in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
