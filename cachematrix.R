## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse property
        inv <- NULL
        
        ## Craete the method to set the matrix
        ## this also resets the inverse variable (inv)
        set <- function( y ) {
                x <<- y
                inv <<- NULL
        }
        
        ## Create the method to grab the matrix
        get <- function() {
                x
        }
        
        ## Create the method to set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        ## Create the method to get the inverse of the matrix (inv)
        getInverse <- function() {
                inv
        }
        
        ## Make available all of the methods via this list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the mean of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse of the matrix has already been created. If so, it gets the 
## matrix mean from the cache and skips the creation.  Otherwise, it 
## creates the inverse of the matrix and sets it in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {

        ## First check to see if the inverse is already created by calling
        ## the getInverse method and storing it in m
        inv <- x$getInverse()
        
        ## if the method isn't null just use the cached matrix and return it
        ## 'return' exits any further execution of the function
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        
        ## We must not have gotten
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        inv <- solve(data) # %*% data
        
        ## Set the inverse to the object
        x$setInverse(inv)
        
        ## Return the matrix
        inv
}
