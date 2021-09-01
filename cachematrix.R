## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a list containing a function to
##1-set the values of the matrix 2-get the values of the matrix 
##3-set the value of the inverse 4-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<- NULL
  }
  get <- function() (x)
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function 
#the following function calculates the inverse of the matrix created with the
##above function. However, it first checks to see if the inverse has already been
##calculated.If so,it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix and set the value of the 
##matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
  
}