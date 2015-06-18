## Caching the Inverse of a Matrix
## The following pair of functions allow for caching of an inverse matrix to
## avoid repeating the costly calculation.

## is.square_matrix takes an input x and checks whether it is a square matrix
## returning TRUE if it is and FALSE otherwise
is.square_matrix <- function(x) {
    if(is.matrix(x) == TRUE){
        if(is.matrix(x) == TRUE){
            return(TRUE)
        }
    }
    return(FALSE)
}

## makeCacheMatrix takes a matrix as input, stores the original and creates
## a variable to hold the cached inverse of the matrix.
## Subfunctions:
##      set: sets the cache for a given input matrix y
##      get: returns the original matrix
##      setinverse: calculates and stores the inverse of the matrix
##      getinverse: returns the cached inverse matrix (initially NULL)
## The subfunctions are contained in a list for access by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    ## Check if the input is a square matrix
    if(is.square_matrix(x) == FALSE){
        message("Please enter a square matrix as input.") 
        return(NULL)
    }
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_result) inverse <<- inverse_result
    getinverse <- function() inverse
    list(set = set, get = get, setinvers = setinverse, getinverse = getinverse)
}

## is.cacheMatrix takes an input x and determines if it is a vector created
## by make CacheMatrix, returning TRUE if it is and FALSE otherwise

is.cacheMatrix <- function(x) {
    if(names(x) = c("set","get","setinverse","getinverse")) return(TRUE)
    else return(FALSE)
}


## cacheSolve takes a vector created by makeCacheMatrix as input and returns
## either a cached inverse if the matrix has not changed and the inverse is
## stored in the cache, otherwise the inverse is calculated, cached, and 
## returned.

cacheSolve <- function(x, ...) {
    if(is.cacheMatrix(x) == FALSE){
        message("Generating cacheMatrix vector for matrix x.")
        x <- makeCacheMatrix(x)
    }
    data <- x$get()
    if((!is.null(inverse)) & (data == x)){
        message("Getting cached inverse matrix.")
        return(x$getinverse())
    }
    message("Taking inverse of matrix.")
    inverse <- solve(data, ...)
    x$setinverse(inverse)
}

