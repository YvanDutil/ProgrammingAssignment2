## This function creates a special "matrix" object that can cache its inverse
##
## It is assumed that the matrix is always invertible

a=matrix(1:4,2,2)


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(toto){
    m <<- solve(toto)
    } 
    
  getinverse <- function() m
  
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- getinverse(x)   #get the cached inverse
  
  #if the cached inverse exist, return the value
    if(!is.null(m)) {
      message("getting cached data")
      return(m)         #return the value and exit the code here
    }
    
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
