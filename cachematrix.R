## Function makeCacheMatrix(x) creates a special "matrix" object that can cache its inverse
## Parameter "x" is matrix
## Returns special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize inverse of matrix with NULL
    m   <- NULL
    
    ## define function to store original matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## define function to retreive original matrix
    get         <- function() x
    
    ## define function to store inverse of matrix
    setsolve    <- function(solve) m <<- solve
    
    ## define function to retreive inverse of matrix
    getsolve    <- function() m
    
    ## return list of defined functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix() 
## Parameter "x" is special "matrix" object
## Returns a matrix that is the inverse of "x"
cacheSolve <- function(x, ...) {
    
    ## retreive cached inverse of matrix
    m <- x$getsolve()
    
    ## if cached inverse of matrix is not NULL, then return it
    if(!is.null(m)) {
        return(m)
    }
    
    ## retrieve original matrix
    data <- x$get()
    
    ## calculate inverse of matrix
    m <- solve(data, ...)
    
    ## cache inverse of matrix
    x$setsolve(m)
    
    ##return inverse of matrix
    m
}
