## This file contains both needed functions to complete the 
## programming assignament 2

## 1) makeCacheMatrix create an especial matrix containing functions 
##    to set/get the matrix and set/get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    # 1. Funtion to set the value of the matrix.
    set <- function(y) {
        x <<- y
        inverted <- NULL
    }

    # 2. Function to get the value of the matrix.
    get <- function() x
    
    # 3. Function to cache the inverted value of the matrix.
    setInverted <- function(inv) inverted <<- inv
    
    # 4. Function to get the inverted cached value of the matrix.
    getInverted <- function() inverted

	# Returns the enviroment with all the functions.
    list(set=set, get=get, 
         setInverted=setInverted, getInverted=getInverted)
}


## 2) Function to compute the inverse of the special matrix created by 
##    makeCacheMatrix and cache it.

cacheSolve <- function(x, ...) {
    inverted = x$getInverted()

    # if the matrix inverted is already cached, return it.
    if(!is.null(inverted)) {
    	message('Getting the inverted matrix from cache')
    	return(inverted)
    }

	# Inverts the matrix
    matrix <- x$get()
    inverted <- solve(matrix, ...)

    # Cache the inverted matrix
    x$setInverted(inverted)

    # Return it
    inverted
}
