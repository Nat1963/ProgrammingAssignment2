## MakeCacheMatrix creates a matrix which can be set (or changed) with 'set' or retrieved
## with the get method.

## MakeCacheMatrix, sets and gets a cached matrix; also caches the inverse of the matrix internally

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}
}


## cacheSolve caculates the inverse of a matrix and caches it. It also returns the
## cached copy of the inverse (if it exists), otherwise returns the calculated inverse
## The calculated inverse is subsequently cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
    
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
