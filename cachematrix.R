## makeCacheMatrix function

library(MASS)  ## library for using "ginv" function

makeCasheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    set <- function(matrix) {
      x <<- matrix
      m <<- NULL
    }
    
    get <- function() x 

    setinv<-function(inverse)m<<-inverse
      
    ## using a matrix multiplication operator, %*%
    getinv <- function() {
      inver <- ginv(x)
      inver%*%x
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

 
## cacheSolve function

CacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...) 
  x$setinv(m)
  m
}

## calculation Test

test <- makeCasheMatrix(matrix(1:6, 2, 3))
test$get()
test$getinv()
CacheSolve(test)
