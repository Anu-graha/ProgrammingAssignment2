## TWo functions are created below. Function makeCacheMatrix creates a special
## matrix object that can cache its inverse and return it. Function CacheSolve 
## makes use of the above created special matrix to get the matrix inverse if already calculated from the cache.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix<-NULL
  
  setMatrix<-function(y)
  {
    x<<-y
  }
  getMatrix<-function()x
  
  setInverse<-function(m){
    inv_matrix<<-solve(m)
  }
  getInverse<-function()inv_matrix
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m<-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else solve(x$getMatrix())
  
}
