## There are two training functions which help to understand
## the scoping rules in R

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) ## this function creates list of functions
{
  
  myInverseMatrix<-NULL ## initialize and set matrix in NULL
  
  setMatrix<-function(y=matrix()) ## function which set matrix 
  {
    x<<-y  ## set x as a matrix which is equal to the input matrix y
    myInverseMatrix<<-NULL  ## initialize and set inverse matrix in NULL 
  } 
  
  getMatrix<-function() x ## return the matrix which is stored in x
  
  setInverseMatrix<-function(InverseMatrix) ## function which set inverse matrix 
  {
    myInverseMatrix<<-InverseMatrix ##set myInverseMatrix as a matrix which is equal to the input matrix 
  }
  
  getInverseMatrix<-function() myInverseMatrix ## return the inverse matrix which is stored in myInverseMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  k<-x$getInverseMatrix() 
  if (!is.null(k)) ## check if inverse matrix exist in cache
  {
    message("getting cached Inverse Matrix") ## return cached matrix
    return(k)
  }
  else ## calculates and stores inverse matrix
  {
    message("getting calculated Inverse Matrix")
    myMatrix<-x$getMatrix()
    myInverseMatrix<-solve(myMatrix)
    x$setInverseMatrix(myInverseMatrix)
    myInverseMatrix
  }
}

