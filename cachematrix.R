## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  returns a list containing a matrix that is capable of caching
#  its inverse thus removing redundant and unnecessary computation.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() i
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#  returns the inverse of matrix which is passed as its
#  first argument.  For the first calculation, the 'inverse' function is called to
#  calculate the returned inverse and the value is cached
#  Subsequent calls return the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}
