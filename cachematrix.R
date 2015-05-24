## The function makeCacheMatrix creates a list of 4 elements, 
## each of the elements can be used to call a function that is
## defined within the makeCacheMatrix function.  The 4 functions
## are explained below:

## set(): allows you to change the stored matrix 'x' to a new matrix
## you specify 'y'.  Also this function resets the value of 'inv'
## to 'NULL'.

## get(): returns the matrix currently stored as 'x'

## setinverse(): allows you to assign a matrix to the value of 'inv'

## getinverse(): returns the matric currently stored as 'inv' or
## returns the value 'NULL' if no matrix is stored.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse=matrix()) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function cacheSolve returns one of two values for the obect 'inv'
## If 'inv' already has matrix stored in itdelf (therefore, not set to 'NULL')
## this function will return the text string 'getting cached data' along with the
## matrix stored in inv.
## If 'inv' is set to the value 'NULL' then this function will use the function solve()
## to return the inverse of the matrix currently set to 'x' and save this inverse matrix
## to the obect 'inv' and will return the value of 'inv' to the console.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  
  inv
}
