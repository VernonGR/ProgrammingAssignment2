
makeCacheMatrix <- function(x = matrix()) {
  n <-NULL
  set <-function(y){  #setting up the matrix
    x <<- y
    n <<- NULL
  }
  get <-function() x
  setmat <-function(solve) n <<- solve # for the inversion of the matrix
  getmat <-function() n
  list(set = set, get = get,  #setting up the list of functions
       setmat = setmat,
       getmat = getmat)
}

cacheSolve <- function(x = matrix(), ...) {
  n <- x$getmat()
  if(!is.null(n)){    #looking for the cached data for the matrix inverstion, in the case it exists. 
    message("getting cached data")
    return(n)
  }
  matrix2 <-x$get()  #if not, then it will invert the matrix using the solve function
  n <- solve(matrix2, ...)
  x$setmat(n)
  n
}