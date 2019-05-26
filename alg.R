normalizar <- function(data, columnas){
  for(i in columnas){
    data[, i] = (data[, i] - min(data[,i]))/(max(data[,i]) - min(data[,i]))
  }
  returnValue(data)
}
maximin <- function(data, columnas,top){
  data_ = data
  fil = length(data[,1]) 
  minimos = c(0, fil)
  for(i in 1:fil){
    mv = data[i, columnas[1]]
    for(j in columnas){
      mv = min(mv, data[i,j])
    }
    minimos[i] = mv
  }
  data_['MV'] = minimos
  data_ = data_[with(data_, order(-minimos) ),]
  returnValue(head(data_, top))
}
minimax <- function(data, columnas, top){
  data_ = data
  fil = length(data[,1]) 
  maximos = c(0, fil)
  for(i in 1:fil){
    mv = data[i, columnas[1]]
    for(j in columnas){
      mv = max(mv, data[i,j])
    }
    maximos[i] = mv
  }
  data_['MX'] = maximos
  data_ = data_[with(data_, order(maximos) ),]
  returnValue(head(data_, top))
}
weighted_average <- function(data, pesos, columnas, top){
  data_ = data
  fil = length(data[,1]) 
  weight = c(0, fil)
  suma = sum(pesos)
  for(i in 1:fil){
    id_p = 1
    w_a = 0
    for(j in columnas){
      w_a = w_a + data[i,j]*(pesos[id_p]/suma)
      id_p = id_p + 1
    }
    weight[i] = w_a
  }
  data_['Weight'] = weight
  data_ = data_[with(data_, order(-weight) ),]
  returnValue(head(data_, top))
}
leximin <- function(data, columnas, top){
  data_ = data
  fil = length(data[,1]) 
  col = length(columnas)
  pesos = matrix(0,fil,col)
  for(i in 1:fil){
    aux = c(0,col)
    id = 1
    for(j in columnas){
     aux[id] = data[i,j]
     id = id + 1
    }
    sort(aux)
    pesos[i,] = aux
  }
  com = matrix(0,col,fil)
  for(i in 1:col){
    com[i,] = pesos[,i]
    data_[paste("V",as.character(i))] = com[i]
  }
  data_ = data_[with(data_, order(-com) ),]
  returnValue(head(data_, top))
}
leximax <- function(data, columnas, top){
  data_ = data
  fil = length(data[,1]) 
  col = length(columnas)
  pesos = matrix(0,fil,col)
  for(i in 1:fil){
    aux = c(0,col)
    id = 1
    for(j in columnas){
      aux[id] = data[i,j]
      id = id + 1
    }
    sort(-aux)
    pesos[i,] = aux
  }
  com = matrix(0,col,fil)
  for(i in 1:col){
    com[i,] = pesos[,i]
    data_[paste("V",as.character(i))] = com[i]
  }
  data_ = data_[with(data_, order(com) ),]
  returnValue(head(data_, top))
}

setwd("C:/Users/Fiorella/Documents/T2-AI")
data = read.csv("cwurData.csv", sep=",", header = TRUE, stringsAsFactors = FALSE, 
                na.strings=c("","NA"))

columnas = c(seq(5,10),12,13) 
pesos = floor(runif(length(columnas), min= 1, max=101))

data = normalizar(data,columnas)
data_maximin = maximin(data,columnas,6)
data_minimax = minimax(data,columnas,10)
data_wa = weighted_average(data, pesos, columnas, 8)
data_leximin = leximin(data, columnas, 5)
data_leximax = leximin(data, columnas, 5)
