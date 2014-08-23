##2 functions: the first to make a chached matrix, 
##and the second to solve the matrix if it has not already been solved

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  im<- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  im <- x$getinverse()
  
  ## check if the matrix has already been solved and return a message if it has been   
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  
  ## if it has not already been calculated, calculate the inverse of the matrix   
  data <- x$get()
  im <- solve(data, ...)
  
  ## set the inverse of the matrix 
  x$setinverse(im)
  im
}

