## Functions to create a matrix and cache its inverse
## Function to create cache matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x

  setinverse <- function(inv){
    inverse <<- inv
  }
  getinverse <- function() inverse

  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## Function to get cached inverse
cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Returning cached inverse")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse
}
