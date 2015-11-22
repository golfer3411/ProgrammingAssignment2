## These two functions, when used in concert with one
## another, take a square invertible matrix as input.
## A list of functions is developed that will set and
## get the input matrix and set and get the inverse of
## the input matrix.  If the input matrix has not 
## changed and the inverse has already been calculated,
## then the cached inverse is returned.  If these
## conditions are FALSE, then the inverse is calculated
## and returned.

## makeCacheMatrix returns a list of 4 functions to set
## and get the matrix and to set and get the inverse of
## the input matrix

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y)       {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates and returns the inverse of the
## matrix input to makeCacheMatrix, unless the input 
## matrix has not changed and the inverse has already
## been calculated.  In this case, it merely retrieves
## the cached inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getinv()
        
        ## Check to see if inverse has already been
        ## calculated
        if(!is.null(inv))       {
                ## Get inverse from cache and do not
                ## calculate
                message("Retrieving cached data")
                return(inv)
        }
        ## If inverse is not cached, calculated the
        ## inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## Assign the inverse matrix via the setinv 
        ## function
        x$setinv(inv)
        
        ## Return the inv
        return(inv)
}
