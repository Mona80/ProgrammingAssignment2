## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix:
## Martrix inversion is usually a costly computation and
## there may be some benefit to catching the inverse of a matrix
## rather than compute it repeatedly.
## Below are a pair of functions that used to create a special 
## object that store a matrix and caches its inverse.

## Write a short comment describing this function
## This function creates a special "matrix" object that could 
## cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i

        
        ## creates list with methods for get / set of both original matrix
        ## and its inverse, and return the list to parent environment
        ## note that this technique allows use of $ operator to access
        ## each  function from the list.
        
             list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## created by makeCacheMatrix above. If the inverse has already 
## been calculated(and the matrix has not changed), then it 
## should retrieve the inverse from the cache

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
