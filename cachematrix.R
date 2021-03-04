## Functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
           
    y <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            y <<- NULL
    }

    ## Get the matrix
    get <- function() {
        m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        y <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        y
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
            m <- x$getInverse()

 
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    m
}
