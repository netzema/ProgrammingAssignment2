## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creats a matrix object which caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Computes the inverse of matrix returned "makeCacheMatrx".
## If Inverse has alreade been calculated, "cachesolve" should get the 
## inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data) %*% data()
        x$setinverse(i)
        i
}
