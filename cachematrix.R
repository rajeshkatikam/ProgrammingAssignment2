##makeCacheMatrix- is used to create a matrix. 
##On succesful execution defines the methods set,get,setInv and getInv. 
##Intializes inverse(i) to NULL
##cacheSolve returns the inverse if already cached.
##Else computes, caches  and returns


##At the matrix creation time inverse(i) is intialized to NULL,set and get
##function are defined
##get() simply returns the current matrix contents,
##set after setting the matrix contents to the passed in matrix,
##resets invers(i) to NULL
##setInv and getInV set and get the inverse (i). 
##Also the list of functions is stored in the list

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

##If the inv(i) is not NULL returns the value in i
##If the inv(i) is NULL compute the inverse and cache it into i.

cacheSolve <- function(x, ...) {
  ## get i 
  i <- x$getInv()
  ##i is not NULL so return the value in i
  if(!is.null(i)) {
    ##This message is printed whenever returning from cache
    message("getting cached data")
    return(i)
  }
  ##i is NULL hence call solve to compute inverse,
  ##cache it and return the inverse in i
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
