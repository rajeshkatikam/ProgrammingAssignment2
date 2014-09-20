##makeCacheMatrix- is used to create a matrix. 
##On succesful execution defines the methods set,get,setInverse and getInverse. 
##Intializes inverse(i) to NULL
##cacheSolve returns the inverse if already cached.
##Else computes, caches  and returns


##At the matrix creation time inverse(i) is intialized to NULL,set and get
##functions are defined
##get() simply returns the current matrix contents,
##set() sets the the matrix contents to the passed in matrix and resets i to NULL,
##setInverse() and getInVerse() set and get the inverse (i). 
##Also the list of functions are stored in the list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i<<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
       
}

##If the inverse(i) is not NULL return the value in i
##If the inverse(i) is NULL compute the inverse and cache it into the variable i.

cacheSolve <- function(x, ...) {
  ## get i 
  i <- x$getInverse()
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
  x$setInverse(i)
  i
}
