## The Functions below can be used to cache the inverse of a
## matrix when it's calculated so it can be used again later
## without calculating it again

## The first function takes a matrix as input and creates a vector
## containing 4 functions to set or get the the matrix and
## to set or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                     
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function(){x}
  setinverse <- function(inverse){inv<<-inverse}
  getinverse <- function(){inv}
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## The second function takes a Vector created with makeCacheMatrix
## and tests with the function getinverse whether the inverse has 
## already been calculated.
## If yes it returns the inverse, otherwise it uses the function get
## to get the matrix and calculates the inverse, then uses the function
## setinverse to set the inverse to the calculated value. When cacheSolve
## is used again on the same Vector it will return the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
