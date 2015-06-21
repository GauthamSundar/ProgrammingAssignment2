## File includes two R functions: 1. makeCacheMatrix & 2. cacheSolve


## The makeCacheMatrix function. makeCacheMatrix returns a list containing 4 functions: 1. setMat 
## 2. getMat 3. setMatInv & 4. getMatInv.

makeCacheMatrix <- function(x) {                ## Function makeCacheMatrix accepts a matrix x
        MatInv <- NULL                          ## Initializing inverse value for future reference
        setMat <- function(y) {                 ## setMat accepts a matrix y as argument and assigns its value to x 
                x <<- y
                MatInv <<- NULL			## sets new matrix inverse to NULL
        }
        getMat <- function() x                  ## getMat returns matrix x
        setMatInv <- function(MI) MatInv <<- MI ## setMatInv accepts matrix inverse i.e. MI as argument and assigns it to variable MatInv 
        getMatInv <- function() MatInv          ## getMatInv returns the matrix inverse, MatInv
        list(setMat = setMat, getMat = getMat,  ## Each function is assigned the same name as itself within the list for convenience when calling the function
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}


## The cacheSolve function. Accepts the above list as argument and calculates matrix inverse if value not already cached.

cacheSolve <- function(u) {                     ## Function cacheSolve accepts above defined list, u as argument
        cachedMatInv <- u$getMatInv()           ## assign MatInv from above function, which may or may not have been calculated already,to cache variable cachedMatInv
        if(!is.null(cachedMatInv)) {            ## verify if MatInv was calculated previously 
                cat("getting cached data...\n") ## print message that inverse has been cached previously  
                return(cachedMatInv)            ## return cached value of MatInv
        }
        data <- u$getMat()                      ## If MatInv has not been calculated, retrieve matrix to perform matrix inverse calculation 
        calMatInv <- solve(data)                ## Calculate matrix inverse and assign to variable calMatInv
        u$setMatInv(calMatInv)                  ## set matrix inverse by pssing variable CalMatInv into appropriate function     
}