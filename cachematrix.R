## Put comments here that give an overall description of what your
## functions do

# creates and manages a cached inverse matrix

## Write a short comment describing this function
#Creates a special "matrix", which is a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse
#assmue the matrix is always square and invertible

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(invM) m <<- invM
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# calculates and caches mean if not already cached
# fetches cached mean if mean already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m

}
