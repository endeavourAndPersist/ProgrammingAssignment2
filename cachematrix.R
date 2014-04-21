## The makeCacheMatix and cacheSolve functions are used in combination to 
## cache the inverse of an invertable matrix in the top level environment



## makeCacheMatrix: accepts an invertable matrix as an argument and wraps
## the set(matrix), get(), setInverse(matrix) and getInverse() functions around 
## it to ## manage the matrix in terms of setting it's value set(matrix), 
## retrieving it's value get() setting it's inverse setInverse(matrix) and 
## retrieving it's inverse getInverse() to and from the environment cache. 

makeCacheMatrix <- function(x = matrix()) {
    mx<-NULL
    set <- function(y = matrix()) {
      x <<- y
      mx <<- NULL
    }
    get <- function() x
    setInverse <- function(inv = matrix()) mx <<- inv
    getInverse <- function() mx
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
  
}


## cacheSolve: Accepts an argument of type list that includes set(),get(),
## setInverse() and getInverse() functions, as is created by the above 
## makeCacheMatrix() function. The list element getInverse is then used to attempt 
## to retrieve an Inverted matrix from the cache, if an inverted matrix exist then 
## it is returned, if not then the original matrix is retrived, inverted(with the 
## solve() function), set with the setInverse() list element (this saves it to the 
## cache) and then it is returned at the call.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mx <- x$getInverse()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setInverse(mx)
  mx
 
}
