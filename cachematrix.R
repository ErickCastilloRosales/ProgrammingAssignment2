# makeCacheMatrix: This function creates a special 
# "matrix" object that can cache its inverse.

## Is really a list containing a function to
## - set the value of the Matrix
## - get the value of the Matrix
## - set the value of the solve Matrix
## - get the value of the solve Matrix

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set<-function(y){
    x<<-y
    Inverse<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) Inverse<<- solve
  getmatrix<-function() Inverse
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# cacheSolve: This function computes the inverse 
# of the special "matrix" returned by makeCacheMatrix 
# above.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(Inverse)){
    message("getting cached data")
    return(Inverse)
  }
  matrix<-x$get()
  Inverse<-solve(matrix, ...)
  x$setmatrix(Inverse)
  Inverse
}
