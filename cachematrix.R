## Matrix inversion can be completed by the following functions

## makeCacheMatrix:a function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
      x<<-y
      i<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) i<<-solve
    getinverse<-function() i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve: a function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
        
}
