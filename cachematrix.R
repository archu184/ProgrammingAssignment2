## Small R code which will inverse the matrix and cache the inverse of matrix rather than compute it repeatedly.
## There are two functions created to cache the inverse of a matrix.

## purpose of makeCacheMatrix function is to   
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inverse<-NULL
  set<-function(y){
  x<<-y
  inverse<<-NULL
}
get<-function() x
setinverse<-function(solve) inverse<<- solve
getinverse<-function() inverse
list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}


# cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse<-x$getinverse()
    if(!is.null(inverse)){
      message("Inverse is getting from cached data")
      return(inverse)
    }
   matrix <- x$get() 
    inverse<-solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}


########### sample testing with 2*2 matrix###########
## > x<-rbind(c(1,2),c(4,5))
## > m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    2
##[2,]    4    5
##> cacheSolve(m)
##[,1]       [,2]
##[1,] -1.666667  0.6666667
##[2,]  1.333333 -0.3333333
##> cacheSolve(m)
##getting cached data
##[,1]       [,2]
##[1,] -1.666667  0.6666667
##[2,]  1.333333 -0.3333333
