## The overall purpose is to use the cached result of matrix inversion because it is CPU intensive process.
## If we want to calculate the inverse of any new matrix, it will calculate it show the answer
## But if inverse of that matrix has been calculated earlier, it will not calculate again and will shoe the cached result

## Two major functions have been used
## 1) makeCacheMatrix 
## 2) cacheSolve

## Description of makeCacheMatrix function:
## It takes a Matrix as input and returns a list of 4 functions, these 4 functions are used in cacheSolve function

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  ## set() function takes a Matrix and store it in a cached environment
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  ## get() function returns the cached Matrix
  get <- function() x
  ## setinv() function stores inverse of the matrix in the cache environment
  setinv<- function(inv) m <<- inv
  ## getinv() function returns the cached inverse Matrix
  getinv <- function() m
  ## These 4 functions are returned as list
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## Description of cacheSolve function:
## It takes a list output from makeCacheMatrix as input and use those function to show the inverse matrix

cacheSolve <- function(x, ...)
{
  ## Checking if the inverse is stored or not, if yes then returning the store value
  m <- x$getinv()
  if(!is.null(m)) 
    {
    message("returning inverse from cached data")
    return(m)
  }
  ## if inverse is stored this part is skipped, otherwise inverse is calculated, displayed and cached 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  message('Calculating first Time')
  return(m)
}

## End of code

## test the code;

x1<-c(1, 8, -9, 7, 5)
x2<-c(0, 1, 0, 4, 4)
x3<-c(0, 0, 1, 2, 5)
x4<-c(0, 0, 0, 1, -5)
x5<-c(0, 0, 0, 0, 1)
x<-rbind(x1,x2,x3,x4,x5)

a<-makeCacheMatrix(x)

cacheSolve(a)
