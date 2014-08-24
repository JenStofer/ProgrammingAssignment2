## cacheSolve either calculates the inverse of a matrix or uses a previously
## stored value if it is available.
## To store the value betwee function calls it uses the four functions
## provided in makeCacheMatrix

## makeCacheMatrix provides four functions to use to cache the value of
## the inverse of a matrix
## set - stores the passed in matrix
## get - returns the value of the stored matrix
## setinverse - stores the inverse of the matrix
## getinverse - returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   ## set inv to NULL when makeCacheMatrix called

        set <- function(y) {
                x <<- y        ## save the matrix passed in
                inv <<- NULL   ## set the inv to NULL
        }

        get <- function() x    ## return the matrix passed in

        setinverse <- function(solve) inv <<- solve   ## save the inverse

        getinverse <- function() inv   ## retrieve the inverse

        ## make the functions accessible
        list( set = set, get = get, setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve either retrieves a previously stored inverse for the given
## matrix. Or it calculates the inverse and returns that while also
## storing the value.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  ## get the stored inverse if available
        if(!is.null(inv)) {    ## was the inverse previously stored?
                message("retrieving cached value")
                return(inv)  ## return the stored value
        }
        ## Only get here if inverse is not stored
        data <- x$get()  ## store the passed in matrix
        inv <- solve(data, ...)  ## solve for the inverse matrix
        x$setinverse(inv)  ## store the inverse matrix
        inv  ## return the inverse matrix
}
