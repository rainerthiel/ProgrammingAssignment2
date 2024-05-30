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
        message("2. Fetch cached inverted matrix data")
        return(minv)
    }
    data <- makem$get()
    minv <- solve(data, ...)
    makem$setminv(minv)
    message("1. Calculate inverse matrix and save in cache")
    minv
}

##
## runTests
##
## This function runs a few examples to test the matrix inversion and caching.
## 
## There are 5 test cases, 3 are expected to work, 2 are expected to fail.
## An inner function solveAndCheck() handles the common stuff.
## A successful test returns the associated identity matrix.
## A test failure reports the error and continues onto the next test.
## 
## Note: I have had to round the identity matrix values (8 significant digits)
## 

runTests <- function() {
    
    matObj <- makeCacheMatrix()                    # instantiate matrix object
    
    solveAndCheck <- function() {               # the common stuff
        
        matObj$set(m1)                          # set the matrix

        tryCatch(                               # wrap to trap and continue
                                                # and continue
            {
                message(label, "Start")
                cacheSolve(matObj)              # calculate inverse and cache
                n1 <- cacheSolve(matObj)        # fetch from cache
                message("3. Calculate and show rounded identity matrix")
                idm <- m1 %*% n1
                print(round(idm, 8))               
                message(label, "*** SUCCESS ***")
            },
            error=function(e) {
                message(label, "*** FAILURE ***")
                print(e)
            }
        )
        
    }

    label <- "Test1: "
    m1 <- matrix(c(1/2,-1/4,-1,3/4),nrow=2)     # discussion example matrix
    solveAndCheck()
    
    label <- "Test2: "
    m1 <- matrix(rnorm(144),nrow=12)            # another example matrix
    solveAndCheck()
    
    label <- "Test3: "
    m1 <- matrix(rnorm(144),nrow=8)             # bad - not a square matrix
    solveAndCheck()
    
    label <- "Test4: "
    m1 <- matrix(c(3,2,0,7,0,1,2,-2,1), nrow=3) # another example matrix
    solveAndCheck()
    
    label <- "Test5: "
    m1 <- matrix(11:26, nrow=4)                 # bad - singular matrix
    solveAndCheck()
    
}
