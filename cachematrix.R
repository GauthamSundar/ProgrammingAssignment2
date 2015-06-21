## File includes two R functions: 1. makeCacheMatrix & 2. cacheSolve
## The function makeCacheMatrix returns a list containing 4 functions: 1. setMat 
## 2. getMat 3. setMatInv & 4. getMatInv.

## Write a short comment describing this function

makeCacheMatrix <- function(x) {            ## Function makeCacheMatrix accepts a matrix x
        MatInv <- NULL                      ## Initializing value for future reference
        setMat <- function(y) {             ## setMat assigns the value of a new matrix y to x 
                x <<- y
                MatInv <<- NULL
        }
        getMat <- function() x              ## getMat prints value of x
        setMatInv <- function(MI) MatInv <<- MI ## setMatInv assigns the matrix inverse of x i.e. MI calculated by function cacheSolve to MatInv 
        getMatInv <- function() MatInv        ## getMatInv prints the matrix inverse of x, MatInv
        list(setMat = setMat, getMat = getMat, ## Each function is assigned the same name as itself within the list for convenience 
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}

## Write a short comment describing this function

cacheSolve <- function(u) {                  ## Function cacheSolve accepts above defined list, u
        cachedMatInv <- u$getMatInv()        ## assign MatInv, which may or may not have been calculated already,to cache variable cachedMatInv
        if(!is.null(cachedMatInv)) {         ## verify if MatInv was calculated 
                cat("getting cached data...\n") ## print message  
                return(cachedMatInv)            ## return cached value of MatInv
        }
        data <- u$get()                      ## If MatInv has not been calculated, retrieve matrix x 
        calMatInv <- solve(data)             ## Calculate matrix inverse of x and assign to variable calMatInv
        u$setMatInv(calMatInv)               ## return matrix inverse CalMatInv
        
}