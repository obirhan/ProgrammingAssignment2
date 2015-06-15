## Cache matrix for programming assignemnt 2 of R programming course of Coursera


## cacheSolve

makeCacheMatrix <- function(x = matrix()) {

    ## inverse of matrix is found via solve builtin function
    inverse <- solve(x)
    
    ## setter of x
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    ## getter of x 
    get <- function() x
    
    ##getter of inverse of x
    getInverse <- function() inverse
    
    ##setter of inverse of x
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
