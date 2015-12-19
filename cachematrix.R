makeCacheMatrix <- function(x = matrix()) {
  #This function takes as input a square invertible matrix
  # This function is more of a constructor function to define four functions :
  #setMatrix(), getMatrix (), getInverse(),setInverse() 
  # The function returns a list of containing the four function objects
  
  
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  #This is the main function that computes and caches the inverse
  # it takes as input x, a list of the four functions defined
  # x$getInverse(), returns the inverse , if the inverse is not computed before
  # then is.null(inv)  is TRUE and the function starts computing the inverse thru 
  # data <- x$getMatrix(), then inv <- solve(data, ...) followed by x$setInverse(inv) 
  # if the inverse is computed before is.null(inv)  = FALSE , an it directly returns
  # the cached inverse without the need to recompute which is the purpose of 
  # the assignment
  
  
  inv <- x$getInverse() #get the Inverse "State"
  
  if(!is.null(inv) ) { # if the inverse is already calculated before it returns the
    #inverse directly 
    message("getting cached data")
    return(inv) 
  }
  
  #inverse is not computed before , and will be computed once 
  data <- x$getMatrix() # get the matrix data 
  inv <- solve(data, ...) # compute the inverse
  x$setInverse(inv) # setting the inverse value for later recall
  inv # returning the inverse matrix to the calling environment
}
