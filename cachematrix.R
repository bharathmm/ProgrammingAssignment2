## The makeCacheMatrix function creates a special matrix that contains
## a list to set and get the matrix, and set and get the inverse of it
## The cacheSolve function computes the inverse of the matrix returned
## by makeCacheMatrix 
## Function makeCacheMatrix has a list to set, get the matrix and
## set and get the inverse of it.

makeCacheMatrix <- function(x = matrix()) 
{
  
  m <- NULL
  set <- function(y)
  { x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function (solve) m <<- solve
  getinv <- function () m
  list(set = set, get = get, setinv = setinv, getinv=getinv)
  

}


## Computes the inverse of matrix. If inverse has already been calculated
##, then it retrieves the inverse from the cache

cacheSolve <- function(x = matrix(), ...) 
{
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data") 
    return (m)}
  
  mat <- x$get()
  m <- solve(mat,...)
  x$setinv(m)
  m
  
}
