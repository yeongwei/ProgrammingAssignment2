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
  isValidInput <- function(input) {
    if (is.null(input)) {
      print("Error! Please supply a matrix argument.")
      x <<- matrix()
      return(FALSE)
    }
    
    if (class(input) != "matrix") {
      print("Error! Please supply a matrix argument.")
      x <<- matrix()
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Constructor
  isValidInput(x)
  inverse <- NULL
  
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
    inverse <<- NULL
  }
  
  getInverse <- function() {
    inverse
  }
  
  setInverse <- function(i, qualifier = "", return = FALSE) {
    # Do not allow user to simply set the inverse
    if(qualifier == "cacheSolve") {
      ret <- NULL
      if (isValidInput(i)) {
        inverse <<- i
        ret <- inverse
      } else {
        ret <- NA
      }
      
      if (return) {
        return(ret)
      }
    } else {
      print("Error! Invalid qualifier.")
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
    print("Error! Please supply a Cachable matrix.")
    return(NULL)  
  }
  
  inverse <- x$getInverse()
  if (is.null(inverse)) {
    inverse<-x$setInverse(solve(x$getMatrix()), match.call()[[1]], TRUE)
  }  
  
  return(inverse)
}
