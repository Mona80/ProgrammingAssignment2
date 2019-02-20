## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i

        
        ## create list with methods for get / set of both original matrix
        ## and its inverse, and return the list to parent environment
        ## note that this technique allows use of $ operator to access
        ## each  function from the list.
        
             list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mdata <- x$get()
        i <- solve(mdata, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
