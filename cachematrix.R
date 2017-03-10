## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix() creates a "special" matrix that can cache its inverse to 
##avoid excess computation effort
##cacheSolve() computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve retrieves the inverse 
#from the cache



##The function makeCacheMatrix take as input a matrix 
##and returns a list of functions that can be applied to the matrix
#these functions set the matrix, get the matrix,set the inverse of matrix 
##and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  A_inv <-NULL
  setMat<-function(B) 
  {
    x<<-B
    A_inv<<-NULL
  }
  getMat<-function() x
  setInv<-function(inv) A_inv<<-inv
  getInv<-function() A_inv
  list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)

}


## Write a short comment describing this function

##cacheSolve() checks if the inverse in the cache of the Matrix is NULL. If yes
##then it prints a message and returns the cached inverse
##Else, it uses the solve() function to compute the inverse and set it as the 
##cache and returns the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  A_inv <- x$getInv()
  if(!is.null(A_inv)) {
    message("getting cached Inverse")
    return(A_inv)
  }
  A_mat <- x$getMat()
  A_inv <- solve(A_mat,...)
  x$setInv(A_inv)
  A_inv
}