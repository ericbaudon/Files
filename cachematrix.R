makeCacheMatrix <- function(x = matrix()) 
{
    elc <- NULL
    set <- function(y)
        {
            x <<- y
            elc <<- NULL
        }
    
    get <- function() x
    setreverse<- function(reverse) elc <<-reverse
    getreverse <- function() elc
    list(set = set, get = get,setreverse = setreverse,getreverse = getreverse)      
}

cacheSolve <- function(x, ...) 
{
    elc <- x$getreverse()
    if (!is.null(elc)) 
    {
        message("getting cached reververse matrix")
        return(elc)
    } else {
        elc <- solve(x$get())
        x$setreverse(elc)
        return(elc)
    }
}

set.seed(12345)
m<-matrix(rnorm(25),ncol=5)
minv=solve(m)
m%*%minv

m_bis<-makeCacheMatrix(m)
minvbis<-cacheSolve(m_bis)
