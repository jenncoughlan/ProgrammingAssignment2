## makeCacheMatrix() takes a matrix, and caches that matrix under the variable x.
##makeCacheMatrix() has different associated imbedded functions, which can be called using the $ 
##(i.e. the get() function, which is passed to the property of x titled 'get' (this is done in the list() function)
##when called, x$get displays the cached matrix x.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,
  setinv=setinv,
  getinv=getinv)

}
## cacheSolve() function uses the cached matrix 'x' and takes the inverse using the solve() function (and stores it as the variable 'i'), 
## if the matrix inverse is not already stored, the cacheSolve() function then caches this inversed matrix within x using the setinv() function.
##If the inverse of the matrix x is already cached, cacheSolve() will print 'getting cached data' when called.
cacheSolve <- function(x, ...) {
    i<-x$getinv()
     if(!is.null(i)){
         message("getting cached data")
         return(i)
     }
     data<-x$get()
     i<-solve(data,...)
     x$setinv(i)
     i
}
