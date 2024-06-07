## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create functions that will cache the inverse of a matrix  

##library(MASS) creates a special matrix object that can cache its inverse

library(MASS)
makeCacheMatrix <- function(x = matrix()){
    inv<-NULL                       #making inverse NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x                #function to get matrix X
    setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
        inver<-ginv(x)
        inver%*%x                   #function to get inverse of matrix
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## this is how we get the cache data by computing the inverse of the matrix

cacheSolve <- function(x, ...)          ##gets cache data
{
    inv<-x$getinv()
    if(!is.null(inv)){                  #checking if inverse is NULL
        message("getting cache data")
        return(inv)     #returns inverse value
    }
    data<-x$get()
    inv<-solve(data...)                 #calculates inverse
    x$setinv(inv)
    inv  ## Return a matrix that is the inverse of 'x'
}
