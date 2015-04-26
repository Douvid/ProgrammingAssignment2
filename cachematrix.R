## The combination of the below 2 functions optimises computation
## by caching some results instead of recalculating again

## The first function creates a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
      n <- 3 ## inputs the parameter for the matrix size
      matrix <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n) ## creates the matrix based on n
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## The second function computes the inverse of the matrix returned by makeCacheMatrix above , only if it has changed else it returns the one cached

cacheSolve <- function(x=matrix(), ...) { ## Return a matrix that is the inverse of 'x'where x is the matrix returned by makeCacheMatrix
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}




