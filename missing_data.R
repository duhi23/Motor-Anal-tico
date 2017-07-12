library(data.table)
library(haven)
library(readr)

datos <- data.table(read_tsv("TemplateCobranzaTotal.txt"))
var_num <- colnames(datos)[as.vector(which(unlist(lapply(datos, class)) == "numeric"))]

perdido <- function(x){
      val <- any(is.na(x))
      return(val)
}

atipico <- function(x){
      val <- all(x < median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)) & all(x > median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE))
      return(val)
}

var_per <- which(unlist(lapply(datos[,var_num, with=FALSE], perdido)))



lapply(var_num, function(i){datos[, (i) := lapply(.SD, perdido), .SDcols=i]})

lapply(atipico, function(i){datos[, lapply(.SD, atipico)]})

"%a%" <- function(x, val){
      x[x > median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)] <- val
      x[x < median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE)] <- val
      return(x)
}


DT[ , lapply(.SD, "%a%", 2)]
cols <- c("V1", "V2")

var <- data.frame(col=c(3,4), valor=c(-100,-200))

for(i in 1:nrow(var)){
      cols <- var[i,1]
      datos[ , (cols) := lapply(.SD, "%a%", var[i,2]), .SDcols = cols]
}


