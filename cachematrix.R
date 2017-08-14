## Put comments here that give an overall description of what your
## functions do

## the function makeCacheMatrix cointains 4 methods. Those methods allow to deal with matrix x, store inverse matrix I and cache it. 
## By calling the set function the inverse matrix I is erased and after calculation replace by new  value.
makeCacheMatrix <- function(x = matrix()){
  
  ## init inverse property
  I <- NULL
  
  ## set the matrix x
  set <- function(m){
    x <<- m
    I <<- NULL
  }
  
  ## get the matrix x
  get <- function(){
    x               
  } 
  
  ## set the inverse matrix of x
  setInv <- function(i) {
    I <<- i
  }
  
  ## get the inverse matrix of x
  getInv <- function() {
    I
  }
  
  ## list of internal methods
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## function calculate a inversion matrix of the 'special' matrix CM. The matrix CM is returned by function makeCacheMatrix().
## If the inverse matrix CM has not been calculated yet or changed, then the inverse matrix IM of the matrix CM is calculated and set 
## and returned as the result. If the matrix CM has been already calculated then it's retrived from the cached valued
cacheSolve <- function( x, ...){
  ## Return a matrix that is the inverse of 'x'
  
  ## the inversion matrix IM of the matrix x
  IM <- x$getInv()
  
  ## calculation of the inversion matrix IM (haven't been calculated)
  ## or has been changed
  if (is.null(IM)) {
    message('Calculation the inverse...')
    
    data <- x$get()
    IM <- solve(data, ...)
    x$setInv(IM)
  } else {
    message('Cached inverse...')
  }
  
  IM
}

