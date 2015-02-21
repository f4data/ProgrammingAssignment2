## These functions provide the inverse of a square invertible matrix
## e.g.: 
## m <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
## x <- makeCacheMatrix(m)
## cacheSolve(x)
## cacheSolve(x)

## Placeholder for a matrix and its inverse value
## Input: original matrix
## Output: none
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Method to store the original matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Method to retrieve the original matrix
        get <- function() x
        
        ## Method to store the inverse matrix
        setinverse <- function(inverse) i <<- inverse
        
        ## Method to retrieve the stored inverse matrix
        getinverse <- function() i
        
        ## List of attributes (methods) available
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a matrix. If it has already been cached retrieves the inverse value.
## Input: makeCacheMatrix (m)
## Output: inverse of matrix m
## The matrix m must be a square invertible matrix
cacheSolve <- function(x, ...) {
        ## Check whether there is already an inverse matrix cached
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting chached data")
                return(i)                
        }
        
        ## If there is no cached inverse matrix, try to compute it
        data <- x$get()
        i <- try(solve(data, ...))
        
        ## Store the new inverse matrix
        x$setinverse(i)
        
        # Print out the value
        i
}
