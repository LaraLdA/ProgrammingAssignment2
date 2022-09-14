
## Proposito: Função R capaz de armazenar em cache cálculos potencialmente demorados


##makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.
#Esta função cria um objeto "matrix" especial que pode armazenar em cache seu inverso.

makeCacheMatrix <- function(x = matrix()) {
    mI <- NULL #inicializa a matriz inversa /initialize the inverse matrix
    
    #define o valor da matriz / set the value of the matrix
    set <- function(y) {
        x <<- y
        mI <<- NULL
    }
    get <- function() x #retorna a matrix (x) / return the matrix (x)
    setmatrixI <- function(inv) mI <<- inv  
    
    getmatrixI <- function() mI # retorna o inverso/return the inverse matrix
    
    list(set = set, get = get,
         setmatrixI = setmatrixI,
         getmatrixI = getmatrixI)
    
  }

##cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#Esta função calcula o inverso da "matrix" especial retornada por makeCacheMatrix acima.
#Se o inverso já foi calculado (e a matriz não mudou), então o cachesolve deve recuperar o inverso pelo cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mI <- x$getmatrixI()
  
  #Verifica se a inversa já foi calculada e retorna / Check if the inverse has already been calculated and returns 
  if(!is.null(mI)) {
    message("Getting cached data")
    return(mI)
  }
  #Get the matrix from our object
  data <- x$get()
  print(class(data)) #print da classe de data / class of data
  
  #calcula o inverso usando a função solve/ calculate the inverse using the solve function
  m <- solve(data)
  
  #Set the inverse to the object/ Define a inversa para o objeto
  x$setmatrixI(m) 
  #retorna a matrix
  m
}