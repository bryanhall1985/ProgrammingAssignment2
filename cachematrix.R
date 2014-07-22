## Functions created for creating a class that can cache a matrix and it's inverse

## This function encapsulates a matrix and it's inverse, if available

makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    ##Basic access methods - get and set 
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


## This function os used to calculate and set the inverse of a makeCacheMatrix object

cacheSolve <- function(x, ...) {
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
        ##return cached inverse matrix
        message("Returning cached inverse")
        return(inversematrix)
    }
        
    ##Solve AX = I
    inversematrix <- solve(datamatrix, diag(nrow=dims[1], ncol=dims[2]), ...)
    x$setinverse(inversematrix)
    inversematrix
}
