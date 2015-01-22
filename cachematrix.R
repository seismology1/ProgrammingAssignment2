#Jesse Kolb
#1/22/2015
#"R Programming" Coursera course programming assignment 2
#Code was adapted from "cacheMean.R" and "makeVector.R" provided in the course.

##makeCacheMatrix: Returns list of functions to cache and retrieve inverted matrices

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y         #double arrows searches through parent environments
        m <<- NULL      #double arrows searches through parent environments
    }
    get <- function() x         #defines get as returning original data
    setinv <- function(solve) m <<- solve       #defines setinv as inverting a matrix
    getinv <- function() m      #defines getinv as returning the inverted matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)       #creates a list of functions to be used in cacheSolve.R
}


##cacheSolve:Retrieves cached inverted matrix if there is one.
##If there isn't, inverts the matrix and caches result.

cacheSolve <- function(x, ...) {
    m <- x$getinv()         #tries to get cached inverse
    if(!is.null(m)) {       #tests if the inverse is cached
        message("getting cached data")
        return(m)       #if the inverse is cached it returns it
    }
    data <- x$get()         #if the inverse is not cached then data becomes data...
    m <- solve(data, ...)       #...and then the inverse is calculated...
    x$setinv(m)         #...and the inverse which was just calculated is now cached
    m       #returns m (the inverse matrix) as the result of the function
}