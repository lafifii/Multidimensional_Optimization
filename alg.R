normalizar <- function(data, columnas) {
  for (i in columnas) {
    data[, i] = (data[, i] - min(data[, i])) / (max(data[, i]) - min(data[, i]))
  }
  returnValue(data)
}

maximin <- function(data, columnas, top) {
  data_ = data.frame(data)
  fil = length(data[, 1])
  minimos = c(0, fil)
  for (i in 1:fil) {
    mv = data[i, columnas[1]]
    for (j in columnas) {
      mv = min(mv, data[i, j])
    }
    minimos[i] = mv
  }
  data_['MV'] = minimos
  data_ = data_[with(data_, order(-minimos)),]
  returnValue(head(data_ , top))
}

minimax <- function(data, columnas, top) {
  data_ = data.frame(data)
  fil = length(data[, 1])
  maximos = c(0, fil)
  for (i in 1:fil) {
    mv = data[i, columnas[1]]
    for (j in columnas) {
      mv = max(mv, data[i, j])
    }
    maximos[i] = mv
  }
  data_['MX'] = maximos
  data_ = data_[with(data_, order(maximos)),]
  returnValue(head(data_ , top))
}

weighted_average <- function(data, pesos, columnas, top) {
  data_ = data.frame(data)
  fil = length(data[, 1])
  weight = c(0, fil)
  suma = sum(pesos)
  for (i in 1:fil) {
    id_p = 1
    w_a = 0
    for (j in columnas) {
      w_a = w_a + data[i, j] * (pesos[id_p] / suma)
      id_p = id_p + 1
    }
    weight[i] = w_a
  }
  data_['Weight'] = weight
  data_ = data_[with(data_, order(-weight)),]
  returnValue(head(data_ , top))
}

leximin <- function(data, columnas, top) {
  data_ = data.frame(data)
  fil = length(data[, 1])
  col = length(columnas)
  pesos = matrix(0, fil, col)
  for (i in 1:fil) {
    aux = c(0, col)
    id = 1
    for (j in columnas) {
      aux[id] = data[i, j]
      id = id + 1
    }
    aux = aux[order(aux)]
    pesos[i,] = aux
  }
  for(i in 1:col){
    data_[paste("v" , as.character(i))] = pesos[,i]
  }
  data_ = data_[with(data_, order(-pesos[,1], -pesos[,2], -pesos[,3], -pesos[,4], -pesos[,5],-pesos[,6]
                                  , -pesos[,7], -pesos[,8], -pesos[,9])),]
  returnValue(head(data_ , top))
}

leximax <- function(data, columnas, top) {
  data_ = data.frame(data)
  fil = length(data[, 1])
  col = length(columnas)
  pesos = matrix(0, fil, col)
  for (i in 1:fil) {
    aux = c(0, col)
    id = 1
    for (j in columnas) {
      aux[id] = data[i, j]
      id = id + 1
    }
    aux = aux[order(-aux)]
    pesos[i,] = aux
  }
  for(i in 1:col){
    data_[paste("v" , as.character(i))] = pesos[,i]
  }
  data_ = data_[with(data_, order(-pesos[,1], -pesos[,2], -pesos[,3], -pesos[,4], -pesos[,5],-pesos[,6]
                                  , -pesos[,7], -pesos[,8], -pesos[,9])),]
  returnValue(head(data_ , top))
  
}

ParetoDomina <- function(a, b) {
  mi = 0
  my = 0
  for (i in 1:length(a)) {
    if (a[i] >= b[i]) {
      mi = mi + 1
    }
    if (a[i] > b[i]) {
      my = my + 1
    }
  }
  
  if (mi == length(a) && my > 0) {
    return(TRUE)
  }
  return(FALSE)
}

skylines <- function(data, l, top) {
  df = data
  df$idx <- 0
  t = length(data[, 1])
  for(i in 1:t) {
    df[i,]$idx = i
  }
  for (i in 1:t) {
    if ( any(df$idx==i) ) {
      a = array(data = 0, dim = length(l))
      for (j in (i + 1):t) {
        if ( any(df$idx==j) ) {
          b = array(data = 0, dim = length(l))
          for (k in 1:length(l)) {
            a[k] = df[i, l[k]]
            b[k] = df[j, l[k]]
            if(is.na(a[k])) {
              a[k] = 0
            }
            if(is.na(b[k])) {
              b[k] = 0
            }
          }
          if (ParetoDomina(a, b)) {
            df <- df[ -j, ]
          }
          else if (ParetoDomina(b, a)) {
            df <- df[ -i, ]
            break
          }
        }
      }
    }
  }
  returnValue(head(df , top))
}



setwd("C:/Users/operador/Downloads")
# setwd("C:/Users/user/Documents/Optimizacion_Multidimensional")
# setwd("C:/Users/Fiorella/Documents/T2-AI")

data = read.csv(
  "cwurData.csv",
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

columnas = c(seq(4, 10), 12, 13)
pesos = floor(runif(length(columnas), min = 1, max = 101))
fil = 100

data = normalizar(data, columnas)
data_maximin = maximin(data, columnas, 20)
data_minimax = minimax(data, columnas, 20)
data_wa = weighted_average(data, pesos, columnas, 20)

data_leximin = leximin(data, columnas, 20)
data_leximax = leximax(data, columnas, 20)
data_skylines = skylines(data, columnas, 20)
