## These functions have the purpose of creating and modifying special
## "cacheMatrix" objects that store not only the matrix itself but also its
## inverse

## makeCacheMatrix creates the cacheMatrix object. The object contains
## functions for setting the contained matrix (set), retrieving it (get),
## saving the inverse (setinverse) and retrieving the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    
    ## i is the inverse of the matrix and is set to NULL when the object is
    ## first created
    i <- NULL
    
    ## this defines the function for setting the matrix
    ## the value of x is set to the content of the matrix
    ## every time the value is set/changed, the inverse is reset to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## this defines the function for getting/printing the matrix
    ## it prints x
    get <- function() x
    
    ## this defines the function for setting the inverse of the matrix
    ## it is supposed to be used by the cacheSolve function
    setinverse <- function(inverse) i <<- inverse
    
    ## this defines the function for getting/printing the inverse
    getinverse <- function() i
    
    ## defines the list of functions contained in the object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve either gets the cached inverse from the cacheMatrix object or
## calculates it and caches it in the object

cacheSolve <- function(x, ...) {
    
    ## gets the inverse from the cacheMatrix object
    i <- x$getinverse()
    
    ## if i is not NULL (i.e., there is already an inverse matrix saved in x),
    ## the (cached) contents are merely printed 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if the inverse had not been saved yet, the matrix is extracted from x and
    ## solved. The inverse is then saved using the setinverse function of
    ## makeCacheMatrix (and also printed)
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}