## The two function work together to take a matrix and perform and inversion on that object
## using the solve(x) function. Becuase this can be a resource intensive process, makeCacheMatrix()
## allows the result of the inversion process performed in cachesolve() to be cached and 
## readily accessible if needed again using the same object instance, rather than creating a 
## second instance of the object. 

## makeCacheMatrix creates an object environment contains the variables/functions x, im, set(), get(),
## setinvmatrix(), and getinvmatrix(). It accepts as x the matrix data to be inverted using cacheSolve()
## The underlying makeCacheMatrix objects can be accesses (along with their underlying stored
## values via alist object that is also created that includes the four variables). 
## The set() function in particular allows for the underlying matrix data to be modified directly,
## without having to duplicate the full object, after an instance of the object has been created.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) im <<- invmatrix
  getinvmatrix <- function() im
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## cacheSolve takes data in the form of that produced by the makeCacheMatrix function and performs the
## matrix inversion, if not previously performed and stored in the makeCacheMatrix() function.
## If previously calculated inverted matrix exists, it pulls and return this object from makeCacheMatrix().
## If the calculated inverted matrix does not exist, the function pulls the matrix data from makeCacheMatrix(),
## inverts the matrix using the solve() funciton, and then caches the output in the makeCacheMatrix() function
## under setinvmatrix() for future access.

cacheSolve <- function(x, ...) {
  im <- x$getinvmatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinvmatrix(im)
  im
}