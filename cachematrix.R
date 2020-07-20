## Functions for constructing, cacheing and inversing a matrix, assuming that the input is 
## an inversible matrix

## Function takes a matrix as input and create a matrix object that can be cached
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function that takes an object created by makeCacheMatrix and returns its inverse matrix
## being it by solving and cashing or by retrieving its value
cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)){
         message("getting cached data for inverse matrix!")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     message("cacheing data for inverse matrix!")
     inv
}