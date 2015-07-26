## x is a square invertible matrix
## The first function, makeCacheMatrix() creates a special "vector",
## which is a list containing a function to
##      1) set the matrix
##      2) get the matrix
##      3) set the inverse
##      4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        matrix <- NULL
        
        set <- function(y){
                
                ## use '<<-' to assign a value to an object in an environment
                ## that is different from the current environment
                
                x <<-y
                
                matrix <<- NULL
        }
        
        get <- function()x
        
        ## m is an argument for inverse matrix
        
        setinv<-function(m)matrix<<-m
        getinv<-function()matrix
        
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}

## cacheSolve function computes the inverse of the original matrix input to makeCacheMatrix(x).
## x is an output of makeCacheMatrix().

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        matrix <- x$getinv()
        
        ## if the inverse has already been calculated.
        
        if(!is.null(matrix)){
                
                ## get it from the cache and skips the computation.
                
                message("getting cached data")
                return(matrix)
        }
        
        ## else, calculate the inverse.
        
        data <- x$get()
        matrix <-solve(data,...)
        
        ## sets the inverse value into the setinv function of the cache.
        
        x$setinv(matrix)
        
        
        matrix
}
