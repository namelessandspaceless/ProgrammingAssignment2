## Functions take inverse of matrix and store them in cache

## instantiates a matrix object, stores info about object in list.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL 
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## Checks to see if inverse of input matrix has been computed, if yes outputs
## results. If not solves the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}