## Put comments here that give an overall description of what your
## functions do
# Below are two functions that are used to create a special object that stores a square invertible matrix and cache's its inverse.
##The cachematrix.R file contains two functions, makeCacheMatrix() and cachemean(). 
##The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
##The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to 
##retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.


## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "vector",
#which is really a list containing a function to set the value of 
#the matrix,get the value of the matrix, set the value of the 
#inverse, get the value of the inverse

## NOTE: please enter an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y = matrix) {
        x <<- y
        i<<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function
#This function first checks to see if the inverse has already been 
#calculated. If so, it gets the inverse from the cache and skips 
#the computation. Otherwise, it calculates the inverse of the data 
#and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
