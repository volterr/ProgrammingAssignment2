## The following functions are the realization of cached solve() function.
## It allows to cache inverse matrix, saving computation time if the results
## will be requested second time

## makeCacheMatrix creates implementation of matrix 
## with ability to cache solve() result
##
## s - cached solve result
## x - matrix data
##
## returns the list of 4 functions:
##  *set - allows to set new matrix data themselves
##  *get - returns matrix data
##  *getsolve - returns cached inverse matrix if any
##  *setsolve - allows to set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve function is the implementation of solve function
## with ability to cache results in the matrix created with makeCacheMatrix function
##
## if input variable x is created with makeCacheMatrix then solve result is cached
## if input variable x is matrix then cacheSolve just returns the solve result
##
## returns inverse matrix

cacheSolve <- function(x, ...) {
    
    if( class(x) == "list" ){   # x was probably created with makeCacheMatrix        
        # check for cached results
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        
        # apply solve function and cache results
        data <- x$get()
        s <- solve(data, ...)     
        x$setsolve(s) 
        
    } else if( class(x) == "matrix" ){  # x is a matrix      
        s <- solve(x, ...)  
        
    } else {
        message("input should be either list created with makeCacheMatrix, either matrix itself")
        s <- NULL
    }
    
    s
}
