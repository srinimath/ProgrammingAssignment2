## By implementing lexical scoping, get the cached inverse of a matrix if the matrix entered is indentical to previous
## else compute inverse


## makeCacheMatrix will create a list of functions to get matrix, set matrix, get inverse matrix and set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    a <<- y
    if(!identical(a,b)){
      x <<- y
      b <<- a
      m <<- NULL
    }        
    
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
  
}


## cacheSolve is the function called to calculate the inverse of matrix..this function first calls vector function from makeCacheMatrix 
## to get the cached inverse matrix if the matrix is identical as the previous call else it will calculate the inverse and store it

cacheSolve <- function(x) {
  
  z <- x
  if(class(z)=="matrix"){
    x<- makeCacheMatrix(x)
    ##check whether the matrix is identical to previous and set matrix inverse (m) based on this condition
    x$set(z) 
    ## get m stored in makeCacheMatrix
    m <- x$getinverse()
    ## if m is not null return cached inverse
    if(!is.null(m)) {
      message("matrix is...")
      print(z)
      print("m is not null")
      message("getting cached inverse matrix...")
      return(m)
    }
    ## if m is null get the value of input matrix and compute inverse
    data <- x$get()
    message("matrix is...")
    print(data)
    m <- solve(data)
    x$setinverse(m)
    message("Inverse is...")
    m
  } else{
    message("Enter a matrix as input")
  }
}

