## These two functions are used to avoid the repeatedly computing
## of the inverse of a matrix, storing the inverse in the cache
## once it is calculated

## This function creates a list of functions to:
## set: set the value of a matrix
## get: obtain the value of the matrix
## setinv: store the inverse of the matrix in the cache
## getinv: obtain the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function gives the value of the inverse of the matrix
## The first time that this function is used, it calculates the inverse
## of the matrix and it saves it in the cache
## Then, this value is get from the cache instead of calculating it again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}