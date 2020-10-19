## Put comments here that give an overall description of what your
## functions do
## These functions are meant help in caching an inverse of matrix
## and reusing it if the matrix does not change

## Write a short comment describing this function
## To create a matrix with special features about it which
## is essentially a list to contain the cache of it's inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
      x<<-y
      inv<<-NULL
  }
  get<-function() x
  setinv<-function(solveMat) inv <<- solveMat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##Function To Retrieve the cache Matrix (i.e inverse calculated) created in the 
##previous function if the matrix has not changed and
##and display the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv  
        ## Return a matrix that is the inverse of 'x'
}
