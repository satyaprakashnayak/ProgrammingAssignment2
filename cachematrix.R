## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix returns a list of four functions
## function set in makeCacheMatrix sets the value of matrix to the value inputted
## function get in makeCacheMatrix gets the value of the matrix
## function setInverse in makeCacheMatrix set the value of Inverse to the cached value
## function getInverse in makeCacheMatrix gets the value of Inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## function cacheSolve returns the inverse of input matrix
## if cached value of inverse exists, it returns that value - along with a message
## 'getting cached data'
## if the the cached value is NULL, the inverse is calculated using 'solve' and set
## using the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
          message ("getting cached data")
          return(m)
        }
        data <- x$get()
        m  <- solve(data,...)
        x$setInverse(m) 
        m
}
