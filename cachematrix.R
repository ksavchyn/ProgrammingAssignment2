## The first function (makeCacheMatrix) creates a special object which can cache the inverse of a matrix; 
## this object includes a list of 4 functions which:
## 
##    set the value of the matrix (set)
##    get the value of the matrix (get)
##    set the value of the inverse of the matrix (setinv)
##    get the value of the inverse of the matrix (getinv)

## The second function (cacheSolve) checks to see if the inverse of the matrix has already been calculated;
## Specifically, it checks if the inverse is already stored in the (makeCacheMatrix) object and if so, it returns
## that value. If the (makeCacheMatrix) object does not contain the inverse, it calculates the inverse by pulling the value of the matrix from the (get) function
## in the (makeCacheMatrix) object and then applying the (inv) function to that data.It then caches the calculated inverse by running the (setinv) function which
## was defined in the (makeCacheMatrix) object.


## The (makeCacheMatrix) function below takes a matrix 'x' as the input and creates
## the 4 functions described above. The (set) function allows you to change the 
## value of and store the matrix, the (get) function outputs the data in the 
## matrix, the (setinv) function assigns 'inv' to the inverse (aka allows you to
## cache the inverse in the object once it's calculated), and the (getinv)
## function outputs the inverse 'inv' value. The output of the function is a list
## containing these 4 functions.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL							
        set <- function(y) {					
                x <<- y						
                inv <<- NULL					
        }
        get <- function() x					
        setinv <- function(inverse) inv <<- inverse	
        getinv <- function() inv				
        list(set = set, get = get,				
             setinv = setinv,
             getinv = getinv)
}


## The (cacheSolve) function below takes a list of functions as the input. It
## sets 'inv' to the output of the (getinv) function as defined in the
## (makeCacheMatrix) object. Then it checks if 'inv' is null or not. If it is not
## null, it prints the message "getting cached data" and returns the value of
## 'inv' stored in the object. Otherwise, it sets 'data' to the output of the
## (get) function as defined in (makeCacheMatrix), sets 'inv' to the result of
## calculating the inverse on 'data' (using the solve function), caches the
## calculated 'inv' in the (makeCacheMatrix) object using the (setinv) function 
## as defined in the object, and returns the 'inv' of 'x'.

cacheSolve <- function(x, ...) {
          inv <- x$getinv()					
        if(!is.null(inv)) {				
                message("getting cached data")	
                return(inv)				
        }							
        data <- x$get()					
        inv <- solve(data, ...)			
        x$setinv(inv)					
        inv		
}
