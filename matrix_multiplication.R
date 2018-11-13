# Read input values function
readinteger <- function(mensaje) {
  n <- readline(prompt = mensaje)
  n <- as.integer(n)
  if(is.na(n)){
    n <- readinteger(mensaje = mensaje)
  }
  return(n)
}
# Split input values for matrix conversion
splitmatrix <- function(val, nrow, ncol){
  aux <- matrix(nrow = nrow, ncol = ncol)
  split <- unlist(strsplit(val, ";", fixed=TRUE))
  for(i in 1:nrow){
    for(j in 1:ncol){
      n <- unlist(strsplit(split[i], " "))[j]
      if(!is.na(n)){
        aux[i,j] <- as.numeric(n)
      }else{
        stop("There is missing values to fill the matrix, 
             please verify your entry ",val)
      }
    }
  }
  return(aux)
}
# Multiply two matrix function
multmatrix <- function(a, b){
  a_row <- nrow(a)
  same_num <- ncol(a)
  b_col <- ncol(b)
  message(a_row)
  message(same_num)
  message(b_col)
  C <- matrix(0, nrow = a_row, ncol = b_col)
  
  for(i in 1:a_row){
    for(j in 1:b_col){
      for(k in 1:same_num){
        aux <- a[i,k]*b[k,j]
        C[i,j] <- C[i,j] + aux
      }
    }
  }
  return(C)
}

a_fila <- readinteger(mensaje = "Give the number of rows for first matrix")
a_colu <- readinteger(mensaje = "Give the number of columns for first matrix")

b_fila <- readinteger(mensaje = "Give the number of rows for second matrix")
b_colu <- readinteger(mensaje = "Give the number of row for second matrix")

if(a_colu != b_fila) {
  stop("The number of columns for the first matrix does not match with the number of rows for the second matrix")
}

a_val <- readline("Give first matrix's values separated by space for columns and semicolon for rows' values")
A <- splitmatrix(a_val, a_fila, a_colu)

b_val <- readline("Give second matrix values separated by space for columns and semicolon for rows values")
B <- splitmatrix(b_val, b_fila, b_colu)

C <- multmatrix(A, B)
