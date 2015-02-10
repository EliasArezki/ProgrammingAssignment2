makeCacheMatrix <- function(x = matrix()) { #This function creates a special "matrix" object that can cache its inverse.
  s <- NULL #begins by setting the inverse s to NULL as a placeholder for a future value
  set <- function(y) { #defines a function which will do the following :
    x <<- y            #sets the matrix, x, to a new matrix, y
    s <<- NULL         #resets the inverse, s, to NULL
  } 
  get <- function() x #returns the vector, x
  setinverse <- function(inverse) s <<- inverse #sets the inverse, s, to inverse
  getinverse <- function() s #return the inverse, s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # returns a "special matrix" containing all of the functions just defined.
}
cacheSolve <- function(x, ...) {#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
  s <- x$getinverse()#sets the inverse to the inverse of the makeMatrix function
  if(!is.null(s)) { #  checks to see if the inverse has already been calculated
    message("getting cached data") # if so, returns the inverse of A, s
    return(s) 
  }
  data <- x$get()# sets in data the matrix to be inversed
  s <- solve(data, ...) # inverses the matrix, assuming it is invertible
  x$setinverse(s) #applies the setinverse function defined above to s
  s
}
