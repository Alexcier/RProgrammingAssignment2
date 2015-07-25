#This Assignment requires two functions
#The first, makeCacheMatrix, sets/gets 
#the value of the matrix
#then proceeds to set/get the value of the matrix

#########################################
#Create Matrix function, setting x, input, to matrix
#begins by setting the ierse to null and then
#opening the set function which immediately sets the inverse
#to null within that environment of the y function
#then the inputs for the set function are set to the same as
#the makeCacheMatrix function
#After this the get, setinverse and getinverse components are set
#finally these are listed out assigning the variables in a list
#########################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               #set inverse
  set <- function(y) {
    i <<- NULL
    x <<- y
  }
  get <- function() x #get matrix
  setinverse <- function(inverse) i <<- inverse #set inverse
  getinverse <- function() i #get inverse of the matrix
  # produce a list of methods
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}
#######################################################################
#Returns "getting cache data" if not null
#returns inverse
#######################################################################
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  #gives feedback that inverse occurred
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #compute the matrix
  data <- x$get() 
  i <- solve(data, ...) %*% data
  #set the inverse to the object 
  x$setinverse(i)
  #return the matrix
  i
}
#test case
#x = rbind(c(4, -1/7), c(-1/9, 2))
#m = makeCacheMatrix(x)
#m$get()
#cacheSolve(m)

#hopefully this commit works....