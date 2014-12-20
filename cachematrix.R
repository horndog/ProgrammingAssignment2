# Function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Holds the cached value or NULL if nothing is cached
  m<-NULL
  
  # Store a matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # Return the matrix
  get<-function() x
  
  # Cache the matrix
  setmatrix<-function(solve) m <<- solve
  
  # Get the cached value
  getmatrix<-function() m
  
  # Return a list of the following elements
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}

cacheSolve <- function(x=matrix(), ...) {
  
  # Get the cached value
  m<-x$getmatrix()
  
  # If it exists return the value
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # If not calculate the inverse and store it
  matrix <- x$get() 
  m <- solve(matrix, ...)
  x$setmatrix(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
