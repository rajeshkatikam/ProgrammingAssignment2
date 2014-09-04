## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix- used to create a matrix. At the matrix creation time inverse(i) intialized to NULL.
##Set and get function are defined. get simply returns the current matrix contents,
##where  as set after setting the matrix contents to passed in matrix resets invers(i) to NULL
##setInv and getInV set and return the inverse (i). 
##Also the list of functions is stored in the list

##cacheSolve gets the inv. If it is NULL it computes the inverse and caches it into i. If not NULL returns i

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i<<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
