## the makeCacheMatrix function has four main roles, which are get the original matrix,
## set the original matrix, get the inverse matrix and set the inverse matrix. Whenever the
## other function use makeCacheMatrix function , it could also use these four functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inver) m <<- inver
  getinverse <- function()m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this fuction is used to compute the inverse of the special "matrix" returned by makeCacheMatrix above 
## by using the solve fuction and after that this function will set the result back to makeCacheMatrix function
## by using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x $ setinverse(m)
  m
}

