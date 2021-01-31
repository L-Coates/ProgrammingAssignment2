## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse. 
#First, it sets the value of the matrix
#Second, it gets the value of the matrix
#Third, it sets the value of the inverse
#Lastly, it gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special matrix returned by the above function, makeCacheMatrix.
##But, if the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve -- the function below -- will retrieve the inverse from the cache, 
##and skip the computation.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
