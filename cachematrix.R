## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inversemx) inversematrix <<- inversemx
    getinverse <- function() inversematrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversematrix <- x$getinverse()
    datamatrix <- x$get()
    ##check to see if matrix is square
    dims<-dim(datamatrix)
    dims
    if (!(dims[1]==dims[2])){
        message("Matrix is not square")
        return(x)
    }
    
    ##check to see if matrix given is empty
    if(is.na(datamatrix[1,1])&length(datamatrix)==1) {
        message("Matrix definition error")
        return(x)
    }
    else if(!is.null(inversematrix)){
        ##get cached
        message("Returning cached inverse")
        return(inversematrix)
    }
        
    
    inversematrix <- solve(datamatrix, diag(nrow=dims[1], ncol=dims[2]), ...)
    x$setinverse(inversematrix)
    inversematrix
}
