## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- solve(x)
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(){
        inverse <<- solve(x)
    }
    list(set = set,get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Takes x as argument cache and solve it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get inverse is called
    inverse <- x$getInverse()
    
    ##check inverse is null or not
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    ##get value of x
    data <- x$get()
    inverse <- solve(data, ...)
    inverse
}
