## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a matrix that
    ## has data and properties which can be cached
    
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
