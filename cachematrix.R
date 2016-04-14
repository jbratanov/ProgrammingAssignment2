#
# makeCacheMatrix:  This function creates a special "matrix" object
# that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix())
  {
    # Initialize matrix object
    mxObj<-NULL
    set<-function(y)
      {
        x<<-y
        # Re-initialize matrix object
        mxObj<<-NULL
      }
    get<-function() x
    setmatrix<-function(solve) mxObj<<- solve
    getmatrix<-function() mxObj
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

#
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#
cacheSolve <- function(x, ...)
  {
    ## Return a matrix that is the inverse of 'x'
    mxObj<-x$getmatrix()
    #
    # If matrix object is not NULL, then go to
    # cache to retrieve cached data
    #
    if(!is.null(mxObj))
      {
        message("getting cached data")
        return(mxObj)
      }
    #
    # Get matrix parameters to compute for
    # solve()
    #
    matrix<-x$get()
    mxObj<-solve(matrix, ...)
    x$setmatrix(mxObj)
    # Display matrix solution
    mxObj
  }
