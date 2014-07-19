#The following functions, will calculate the inverse of a matrix once and store in cache so that
#it will not need to be calculated if it is needed subsequent times

#makeCacheMatrix creates a special matrix, which is really a list containing a function to 
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {                   # Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                    #get the value of the matrix
        setinv <- function(solve) m <<- solve  # set the value of the inverse
        getinv <- function() m                 #get the value of the inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If 
#the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
        m <- x$getinv()          
        if(!is.null(m)) {              #Check to see if the inverse has already been calculated
                message("getting cached data")
                return(m)              #If the inverse has been calculated, retrieve it from the cache
        }
        data <- x$get()   
        m <- solve(data, ...)          #If the inverse has not been calculated, calculate it now
        x$setinv(m)                    #and store it in the cache        
        m                              #Return a matrix that is the inverse of 'x'
}
