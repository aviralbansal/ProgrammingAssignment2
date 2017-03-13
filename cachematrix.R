## These two funtions help create a matrix object that is aware of its inverse and returns the inverse when requested ensuring that the calculation is done only if the inverse of the matrix has not been calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        if(!is.matrix(y)) stop("input must be a matrix")
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setInverse<-function(invrs) inv<<-invrs
    getInverse<-function() inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mtrx<-x$get()
    inv<-solve(mtrx,...)
    x$setInverse(inv)
    inv
}
