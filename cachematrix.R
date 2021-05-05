## Put comments here that give an overall description of what your
## functions do
    ##x is a square matrix
##set the matrix
##get the matrix
##set the inverse of the matrix
##get the inverse of the matrix
    

## Write a short comment describing this function
##The function makeCacheMatrix creates a special matrix x object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inv=NULL
                set=function(a){
                  x<<-a
                  inv<<-NULL
                
                }
                get<-function()x
                setinv<-function(inverseofamatrix)  inv<<-inverseofamatrix 
                getinv=function()inv
                list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##The function cachesolve returns the inverse of the matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
        return(inv)
        
      }
      
      mat.data<-x$get()
      inv<-solve(mat.data, ...)
      x$setinv(inv)
      return(inv)
}
