## Put comments here that give an overall description of what your
## functions do

## This is a set of functions get and set the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv_mat) m <<- inv_mat
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function will first check to see if the inverse
#  matrix is saved in cache, If yes return matrix, if no
#  calculate the inverse and save to cache. Function
#  makeCacheMatrix gets and sets the inverse matrix in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
