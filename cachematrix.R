## Put comments here that give an overall description of what your
## functions do
## because sometimes when data is too big, doing an operation is too time-consuming,
## what this does is that if the matrix doesnt change,you do not need to recalculate
## everything, instead, you grab the value from the cache (the operation that you
## already did with the same matrix).

## Write a short comment describing this function
## the function sets and gets the value of the matrix and then sets and gets the
## value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ma<- NULL
        set<- function(y){
                x<<- y
                ma<<- NULL
        }
        get<- function() x
        inverting <- function(solve) ma<<- solve 
        getinverse <- function() ma
        list(set=set, get=get,
             inverting= inverting,
             getinverse= getinverse)
}


## Write a short comment describing this function
## this function checks if the inverse matrix is already calculated, if it is, it brings
## the value already calculated, if not it calculates it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ma<- x$getinverse()
        if(!is.null(ma)) {
                message ("getting cached data")
                return (ma)
        }
        data<- x$get()
        ma<- solve(data, ...)
        x$inverting(ma)
        ma
}
