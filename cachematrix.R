## 
## The makeCacheMatrix and cacheSolve functions below together
## implement a matrix object that can compute and cache its
## inverse for later access.
## 
## Usage vignette:
##
## Create a cachingMatrix object from an invertible matrix m:
##   mcache <- makeCacheMatrix(m)
## Compute the matrix inverse and cache it for later access:
##   minv <- cacheSolve(mcache)  # 1st call computes and returns inverse
##   minv <- cacheSolve(mcache)  # now the cached value is returned
## Retrieve the original matrix:
##   morig <- sm$get()
## Reset the cachingMatrix to use a different matrix:
##   mcache$set(newMatrix)
##

## The makeCacheMatrix function creates a cachingMatrix object 
## that can cache its inverse. The retured object defines the 
## following four functions:
##   obj$get()  - returns the original matrix provided when
##                obj was created, or that was later set.
##   obj$set(m) - sets the original matrix to m.
##   obj$getinverse()  - returns the cached inverse, if available,
##                       else NULL
##   obj$setinverse(m) - sets the cached matrix to m.
makeCacheMatrix <- function(origm = matrix()) {
    inversem <- NULL
    set <- function(y) {
        origm    <<- y
        inversem <<- NULL
    }
    get <- function() origm
    setinverse <- function(matinv) inversem <<- matinv
    getinverse <- function() inversem
    
    # Return a list of functions to get & set the 
    # original matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the cachingMatrix
## returned by the makeCacheMatrix function. If the inverse has not
## been computed, cacheSolve computes, caches and returns it. If
## the inverse has been cached, the cached solution is returned.
cacheSolve <- function(cachingMatrix, ...) {
    stopifnot(is.list(cachingMatrix))
    stopifnot(is.function(cachingMatrix$getinverse))
    m <- cachingMatrix$getinverse()
    if(!is.null(m)) {
        message("...retrieving cached inverse")
        return(m)
    }
    message("...computing and caching inverse")
    data <- cachingMatrix$get()
    m <- solve(data, ...)
    cachingMatrix$setinverse(m)
    m
}

## This function tests the cacingMatrix functionality. An error is 
## generated if any test fails; otherwise the fuction returns TRUE. 
testCachingMatrix <- function() {
    testm    <- matrix(1:4, 2, 2)
    testm2   <- matrix(5:8, 2, 2)
    testinv  <- solve(testm)
    testinv2 <- solve(testm2)
    
    message("Testing makeCacheMatrix...")
    cm <- makeCacheMatrix(testm)
    stopifnot(is.function(cm$get))
    stopifnot(is.list(cm))
    stopifnot(is.function(cm$set))
    stopifnot(is.function(cm$getinverse))
    stopifnot(is.function(cm$setinverse))
    
    message("Testing cachingMatrix.get()/set()...")
    stopifnot(is.matrix( cm$get() ))
    stopifnot(identical( cm$get(), testm ))
    
    cm$set(testm2)
    stopifnot(is.matrix( cm$get() ))
    stopifnot(identical( cm$get(), testm2 ))
    
    message("Testing cachingMatrix.getinverse()/setinverse()...")
    stopifnot(is.null( cm$getinverse() ))
    
    cm$setinverse(testinv2)
    stopifnot(is.matrix( cm$getinverse() ))
    stopifnot(identical( cm$getinverse(), testinv2 ))
    
    cm$set(testm)
    stopifnot(identical( cm$get(), testm) )
    stopifnot(is.null( cm$getinverse() ))
    
    message("Testing cacheSolve...")
    stopifnot(identical( cacheSolve(cm),  testinv ))
    stopifnot(identical( cm$getinverse(), testinv) )
    stopifnot(identical( cacheSolve(cm),  testinv ))

    cm$set(testm2)
    stopifnot(identical( cm$get(), testm2) )
    stopifnot(is.null( cm$getinverse() ))
    stopifnot(identical( cacheSolve(cm),  testinv2 ))
    stopifnot(identical( cm$getinverse(), testinv2) )
    stopifnot(identical( cacheSolve(cm),  testinv2 ))
    
    message("Testing cacheSolve caching...")
    # capture R message text (really stderr)
    txtConn <- textConnection("foo", "w")
    sink(txtConn, type = "message", append=T)
    m <- cacheSolve(cm)
    sink(type = "message")
    close(txtConn)
    message(paste("...message was:", foo, "\n"))
    stopifnot(identical( m,  testinv2 ))
    stopifnot(is.integer( grep('retrieving', foo) ))
    
    TRUE
}
