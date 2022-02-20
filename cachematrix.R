## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix, makeCacheMatrix
##makeCacheMatrix consist of set , get,  setinv, getinv
##library(Mass) is used to calculate inverse for non squared as well as square  matrices
## Write a short comment describing this function


library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 #intilaizing inverse as well 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }           
  #function to get matrix x
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){ 
    inver<-ginv(x)
    inver%*%x              #function to obtain inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##this is used to get the cache data

cacheSolve <- function(x, ...)   #get cache data
  {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {        #checking whether inverse is null
    message("getting cached matrix")
    return(inv)              # returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)     #calculate inverse value 
  x$setinv(inv)
  inv
  
}



