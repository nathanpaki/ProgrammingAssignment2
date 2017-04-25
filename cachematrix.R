## Put comments here that give an overall description of what your
## functions do
## Returns a list containing Set the Matrix, Get the Matrix, 
## Set the inverse, get the inverse
## 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
          set <- function(y) {
          x <<- y
          inv <<- NULL
        }  
        
        get <- function() x 
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
 
}


## Write a short comment describing this function
## returns inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}