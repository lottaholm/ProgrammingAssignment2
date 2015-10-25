## Below are two functions "makeCacheMatrix" and "cacheSolve". 
## The first function ("makeCacheMatrix")takes the matrix you want to inverse as input and also contains subfunctions that store and get the matrix and it's inverse from cache.
## The second function ("cacheSolve") calculates the inverse and returns it. However, if there already is a cached inverse (and the original matrix has not changed since) it returns the cached version.


## The "makeCacheMatrix" function below creates a matrix. It also includes five subfunctions that can be called upon separately if you need to check the content of your matrix or update it. 


makeCacheMatrix <- function(x = matrix()) {
      
    ## Calling only makeCacheMatrix assigns the input value x (the original matrix) to the variable "currentmatrix"
    currentmatrix <<- x
    
    ## The function "setnewmatrix" changes the vector stored in the main function. "currentmatrix" now takes the value of "newvaluesformatrix".
    ## This function is only used if you want to change the values of the matrix.
    ## You call it using e.g. x$set(c(7,4,1,2)) where "x" previously had been assigned content using e.g. x <- CacheMatrix(c(1,2,3,4), 2, 2)    
    setnewmatrix <- function(newvaluesformatrix) {
        currentmatrix <<- newvaluesformatrix        ##NB! The <<- is needed so that the changes are saved to "currentmatrix" also outside the "setnewmatrix"-function.
    }
    
    ## The "getcurrentmatrix" and "getcachedmatrix"-functions return the current value of the matrix or its inversere. They don't require any input,.
    ## You simply call them using x$getcurrentmatrix() or x$getcachedmatrix where "x" is the special vector that has been assigned content using e.g. x <- CacheMatrix(c(1,2,3,4), 2, 2)
    getcurrentmatrix <- function() {
        currentmatrix
    }
    getcachedmatrix <- function() {
        cachedmatrix
    }
     
    ## The function "getvalueofinversematrix" returns the cached inverse matrix
    getcachedinversematrix <- function() {
        cachedinverse
    }   
    
    ## The function savematrices  saves the value of both the current matrix (currentmatrix) and the inversed matrix (currentinverse) to the cache.
    savematrices <- function(newinverse) { 
        cachedinverse <<- newinverse           ##NB! The <<- is needed so that the changes are saved to "currentmatrix" also outside the "savematrices"-function.
        cachedmatrix <<- currentmatrix          ##NB! The <<- is needed so that the changes are saved to "cachedmatrix" also outside the "savematrices"-function.
    }
       
    ## To store the four functions we need a function list so that when we assign makeVector to an object, the object has all the four functions:
    list(setnewmatrix=setnewmatrix, getcurrentmatrix=getcurrentmatrix, getcachedmatrix=getcachedmatrix, savematrices=savematrices, getcachedinversematrix=getcachedinversematrix)
           
        
}


## The following function called "cacheSolve" is the function that actually calculates the inverse. 
## Before doing so, it checks if the original matrix (the one we want to inverse) is up to date (or if the user has passed a new matrix but not yet calculated the inverse)

cacheSolve <- function(x, ...) {
    ## "extracting" the values of the currentmatrix (the latest that the user has "submitted") and the cachedmatrix.
    currentmatrix <- x$getcurrentmatrix()
    cachedmatrix <- x$getcachedmatrix()

    ## The next if-statement checks if the currentmatrix and the cachedmatrix are the same, or if the value of the matrix (i.e. currentmatrix) has changed since it's inverse last was cached
    if(identical(cachedmatrix, currentmatrix)) {       
        message("getting cached inverse")
        return(cachedinverse) ## the cached inverse is returned and the function is ended        
    } 
    
    ## if the cachedmatrix is not up to date, we calculate the new inverse:
    newinverse <- solve(currentmatrix)  ## the solve()-function calculates the inverse of a matrix
    newinverse                          ## prints out the new inverse
    x$savematrices(newinverse)          ## saves the new inverse in the cache
        
} 
        
        
        
        
        
        
        }
