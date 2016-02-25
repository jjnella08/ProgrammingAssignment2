## Caching the Inverse of a Vector
## Below are two functions that are used to create a
## special object that stores a numeric vector and caches its inverse.

## This functioncreates a special "matrix" object that can cache 
## its inverse.
## It returns a list containing a function to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){  ## Function to assign a value to the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                              ## Function to return the value of the matrix argument (x)
  setinverse <- function(inverse) inv <<- inverse  ## assign value to inv in this environment
  getinverse <- function() inv                     ## returns the value of inv
  
  list(set = set, get = get,  ## create a list of the above functions
       setmean = setmean,
       getmean = getmean)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        
  inv <- x$getinverse()  ## assign the inverse of the matrix x to inv
  
  if(!is.null(inv)){ ## check if inv is null, if not use cached data
    message("getting cached data")
    return(inv)
  }
  
  mdata <- x$get()          ## call the get function to get the matrix data
  inv <- solve(mdata, ...)  ## use the solve function to greate the inverse of the matrix data
  x$setinverse(inv)         ## set tthe inverse value
  inv                       ## return the inverse
}
