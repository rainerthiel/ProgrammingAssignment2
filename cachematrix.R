##
## This is programming assignment 2 of the Data Science Specialisation
## course module "R Programming" https://class.coursera.org/rprog-013
##-----------------------------------------------------------------------
## makeCacheMatrix
##
## This function creates a "special" matrix vector x and exposes it
## through 4 functions that are returned as a list:
## 1. get() returns the input matrix x
## 2. set(y) assigns y, the matrix value argument to x
## 3. getminv() returns inv, the cached inverse of matrix x
## 4. setminv(minv) assigns the matrix inverse calculation minv to inv
##
## Essentially makeCacheMatrix is a wrapper object for
## x - the input matrix (assumed to be invertible)
## y - the inverse of matrix x
## associated getter and setter methods
##
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setminv <- function(minv) inv <<- minv
    getminv <- function() inv
    list(set = set, get = get,
         setminv = setminv,
         getminv = getminv)
}


##
## cacheSolve
##
## This function calculates the inverse of the matrix
## returned by the makeCacheMatrix$get function.
## It also caches the inverse result so that it can be returned 
## directly on subsequent calls without having to recalculate it.
##
## This function depends on an instance of the makeCacheMatrix function,
## passed in as mandatory argument makem.
##
cacheSolve <- function(makem, ...) {

    minv <- makem$getminv()
    if(!is.null(minv)) {
        message("getting cached inverted matrix data")
        return(minv)
    }
    data <- makem$get()
    minv <- solve(data, ...)
    makem$setminv(minv)
    minv
}
