## These two functions allow to calculate the inverse of a matrix x and store its value in the cache.
## If the value has been previously calculated, it can be retrieved from the cache without performing 
## the calculation again. 


## This function creates a an object, based on the matrix x and its inverse, and stores it in the cache.
## The initial matrix x can be substituted with a new matrix via the set function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) {inv <<- solve}
        getinverse <- function() {inv}
        
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)          
}


## This function checks if the inverse of a matrix has been already calculated. If the response is positive,
## it returns the inverse preceeded by the message :"please wait, getting cached data for you!"

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("please wait, getting cached data for you!")
                return(inv)
        }
        
## If the response of the above query is negative, then the inverse of the matrix x is now calculated and stored in the cache.
        
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinverse(inv)
        inv
}
