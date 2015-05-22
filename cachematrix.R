## Creates a Matrix object with enriched setters and getters
##
## Args:
##  x: Input of matrix class
##
## functions:
##  getMatrix: Returns the matrix within the object
##  setMatrix: Sets a valid matrix for the object else defaults to empty matrix
##  getInverse: Returns the inverse matrix
##  setInverse: Sets the inverse matrix; If return is TRUE then return the value
##

makeCacheMatrix <- function(x = matrix()) { 
  # Privates
  isSquareMatrix <- function(x) {
    if (ncol(x) == nrow(x)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  isValidInput <- function(input) {
    if (is.null(input)) {
      warning("Please supply a matrix argument.")
      x <<- matrix()
      return(FALSE)
    }
    
    if (class(input) != "matrix") {
      warning("Please supply a matrix argument.")
      x <<- matrix()
      return(FALSE)
    }
    
    if (!isSquareMatrix(x)) {
      warning("Please supply a square matrix.")
      x <<- matrix()
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  setSize <- function(x) {
    return(ncol(x))
  }
  
  # Constructor
  isValidInput(x)
  inverse <- NULL
  size <- setSize(x)
  
  # Publics
  getMatrix <- function() {
    x
  }
  
  setMatrix <- function(m = matrix()) {
    if (isValidInput(m)) {
      x <<- m
    } else {
      x <<- matrix()
    }
    size <<- setSize(x)
    inverse <<- NULL
  }
  
  getInverse <- function() {
    inverse
  }
  
  setInverse <- function(i, return = FALSE) {
    ret <- NULL
    status <- identical(diag(size), x %*% i)
    if(status) {
      if (isValidInput(i)) {
        inverse <<- i
        ret <- inverse
      } else {
        ret <- NA
      }
    } else {
      warning("Not a valid inverse matrix")
    }

    if (return) {
      return(ret)
    }
  }
  
  # Accessors
  accessors <- list(getMatrix = getMatrix, setMatrix = setMatrix,
       getInverse = getInverse, setInverse = setInverse)
  return(accessors)
}


## Compute the inverse matrix based on the given cachable matrix object
## If inverse matrix is null then compute
##
## Args:
##  x: Cachable matrix object to perform inverse matrix computation
##

cacheSolve <- function(x, ...) {
  if (is.null(x)) {
    warning("Please supply a Cachable matrix.")
    return(NULL)  
  }
  
  inverse <- x$getInverse()
  if (is.null(inverse)) {
    m <- x$getMatrix()
    i <- solve(m)
    inverse<-x$setInverse(i, TRUE)
  }  
  
  return(inverse)
}
