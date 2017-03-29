# Function makeCacheMatrix returns a list of functions to get and set 
# the matrix and its inverse matrix values.
# This function does not do the inverse matrix computation by itself 
# however handles the caching of data and its retrieval

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
 set<-function(y){
         x<<-y
         m<<-NULL
 }
 get<-function()x
 setinverse<-function(solve) m<<-solve
 getinverse<-function() m
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Function cacheSolve uses solve function to generate inverse of the matrix 
## if the inverse is not already cached. 
## If the result is already cached it is retrieved and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)

        }
        data <- x$get()
        message("computing inverse")
        m <- solve(data)
        message("caching inverse")
        x$setinverse(m)
        m
}
