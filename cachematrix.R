## The function makeCacheMatrix first creates an empty matrix as a 
## formal parameter. In the body of the fucntion another the
##fucntion set allows the end user to pass a matrix variable to 
## function. The function returns a list containing the results of ## the subfunctions set, get setInverse and get invers

makeCacheMatrix <- function(x = matrix()) {
	result_m <- NULL
        
        set <- function(y){
                x <<- y
                result_m <<- NULL
        }
        
        
        get <- function() x
        
        setInverse <- function(solve) result_m <<- solve
        
        getInverse <- function() result_m
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)

}


## This function receives the special matrix created with the above ## function to cache and uses the cached fucntions to get the matrix ## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        result_m <- x$getInverse()
        
        if(!is.null(result_m)) {
                message("getting cached data")
                return(result_m)
        }
        
        data <- x$get()
        result_m <- solve(data, ...)
        x$setInverse(result_m)
        result_m
}

