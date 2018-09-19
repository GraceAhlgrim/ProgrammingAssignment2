## The makeCacheMatrix stores the matrix and the inverse of the matrix as a R object. 
## The matrix inverse and the list of functions( "set", "get, "setinverse", "getinverse") 
## are stored in the environment of the R object created by makeCacheMatrix to then be 
## retrieved by the cacheSolve function.
## The cacheSolve function searches for the cached data from the R object and prints the 
## inverse matrix.

## makeCacheMatrix calculates the inverse of the given matrix and receives (or gets) 
## the information to save in the R Object's environment. It is important to note though that the 
## makeCacheMatrix function is incomplete unless it is used with the cacheSolve function 
## which actually retrieves the inverse and prints the result.

makeCacheMatrix <- function(x = numeric()) {
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




## cacheSolve is a function that produces the inverse of the matrix after the original 
## matrix has been run through the makeCacheMatrix for the cached data. 
## The lexical scoping of R allows for the function cacheSolve to find the list that defines
## "get", "getinverse", "set", and "setinverse" that was created in the function 
## makeCacheMatrix because cacheSolve uses the R object that was created through the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
