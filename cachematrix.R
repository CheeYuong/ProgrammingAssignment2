## x is a square invertible matrix
## The first function, makeCacheMatrix() creates a special "vector",
## which is a list containing a function to
##      1) set the matrix
##      2) get the matrix
##      3) set the inverse
##      4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        matrixinv <- NULL
        
        set <- function(y){
                
                ## use '<<-' to assign a value to an object in an environment
                ## that is different from the current environment
                
                x <<- y
                
                matrixinv <<- NULL
        }
        
        get <- function()x
        
        setinv<-function(inverse)matrixinv <<- inverse
        getinv<-function()matrixinv
        
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}

## cacheSolve function computes the inverse of the original matrix input to makeCacheMatrix(x).
## x is an output of makeCacheMatrix().

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        matrixinv <- x$getinv()
        
        ## if the inverse has already been calculated.
        
        if(!is.null(matrixinv)){
                
                ## get it from the cache and skips the computation.
                
                message("getting cached data")
                return(matrixinv)
        }
        
        ## else, calculate the inverse.
        
        data <- x$get()
        matrixinv <-solve(data,...)
        
        ## sets the inverse value into the setinv function of the cache.
        
        x$setinv(matrixinv)
        
        
        matrixinv
}
