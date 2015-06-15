## The following is the two functions that are used to create a special object
## that stores a numeric vector and cache's its mean.


## This function crates a special matrix object that can caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() return(X)
  setinv<-function(inv) inverse<<-inv
  getinv<-function() return(inverse)
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function compute the inverse of the special matrix 
## created with the above function.
## It first checks to see if the inverse is already computed.
## If so,it gets the inverse from the cache and skip the computation.
## Otherwise, it computes the mean of the data and sets the inversed matrix 
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinv(inverse)
  inverse
}