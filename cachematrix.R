## The makeCacheMatrix() function serves as an accessor function generator and a cache. 
## It accepts a square (invertible) matrix and returns a list, where each member of the list 
## are accessor functions. It additionally declares a cached variable containing the 
## matrix inverse, such that on subsequent attempts to calculate the inverse the cached value can be 
## returned. The get() function retunrs the original matrix and the getinverse() 
## function returns its inverse. The set() function sets the matrix(and nullifies the inverse cache)
## while the setinverse() functions actually sets the inverse.
##
## The cacheSolve() function accepts an instance of the list returned by makeCacheMatrix() and 
## attempts to use the generated functions to either return a previously cached inverse value
## (if the same list instance is passed) or calculate it and return it for the first time.


## The makeCacheMatrix() function serves as a cache holder for a matrix inverse and provides accessor 
## functions to access it

makeCacheMatrix <- function(mat = matrix()) {
  
  ## initialize the cache to null
  inv <- NULL
  
  ## set the cache to null and initiaize the source matrix
  set <- function(y) { 
    mat <<- y
    inv <<- NULL
  }
  
  ## get the source matrix
  get <- function(){
    mat
  }
  
  ## set the inverse
  setinverse <- function(invvalue) {
    inv <<- invvalue
  }
  
  ## get the calculated inverse
  getinverse <- function(){
    inv
  } 
  
  ## return a list of the accessor functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Accepts an instance of the list returned by makeCacheMatrix() and 
## attempts to use the generated functions to either returned a previously 
## cached inverse value or calculate it and return it for the first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## attempt to get a cached inverse
  inv <- x$getinverse()
  
  ##if no cached value (i.e. we have not calculated it yet)
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse
  inv <- solve(data, ...)
  
  ## cache the inverse for future access
  x$setinverse(inv)
  
  ## return the inverse
  inv
  
}
