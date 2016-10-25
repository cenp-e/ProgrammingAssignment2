## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y){
        x<<- y
        inv<<-NULL
    }
    get= function() x
    setinv= function(inverse) inv<<- inverse
    getinv= function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## This function returns the inverse of the matrix. By first checking if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
##setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv = x$getinv()
    
    if (!is.null(inv)){
        
        message("getting cached data")
        return(inv)
    }
    
    
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    
    return(inv)
}
