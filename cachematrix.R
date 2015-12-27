##This function stores a cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        
        # Returns inverse matrix of x
        getInverse <- function()inv
        
        # Sets and STORES the inverse matrix of x
        setInverse <- function(tempInverse){
                inv <<- tempInverse 
        }
        
        # Returns the original matrix (x)
        get <- function()x
        
        # Sets a new matrix (x) and resets the inverse to NULL
        set <- function(matx){
                x <<- matx
                inv <<- NULL
        }
        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse. If already computed, it simply returns the inverse stored in cache. 

cacheSolve <- function(x, ...) {
        
        # Determines if the inverse has already been calculated
        if(is.null(x$getInverse())) {
                
                # If inverse is null (not calculated) solves and stores it in x
                solve(x$get()) -> solvInv
                x$setInverse(solvInv)
        }
        
        # Before exiting, returns inverse value
        x$getInverse()
}

