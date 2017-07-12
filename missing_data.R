library(data.table)
library(haven)
library(readr)

datos <- data.table(read_tsv("TemplateCobranzaTotal.txt"))
vars <- colnames(datos)[as.vector(which(unlist(lapply(datos, class)) == "numeric"))]
var_num <- as.vector(which(unlist(lapply(datos, class)) == "numeric"))


perdido <- function(x){
      val <- any(is.na(x))
      return(val)
}

atipico <- function(x){
      #val <- all(x < median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)) & all(x > median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE))
      val <- any(x > median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)) | any(x < median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE))
      return(val)
}

var_per <- which(unlist(lapply(datos[,vars, with=FALSE], perdido)))
# var_per deberia ser un data.frame que a mas de contener el numero de la columna
# que presenta valores perdidos, debe poseer el valor a reemplazar
for(i in 1:length(var_per)){
      cols <- tabla[i,1]
      datos[ , (cols) := lapply(.SD, "%p%", tabla[i,2]), .SDcols = cols]
}


var_atp <- which(unlist(lapply(datos[,vars, with=FALSE], atipico)))
# var_atp deberia ser un data.frame que a mas de contener el numero de la columna que
# presenta valores atipicos, debe poseer el valor a reemplazar o a su vez eliminar

# var_num[as.vector(var_atp)] # posiciones de las variables en la base original
# tabla <- data.frame(col=c(14:37,50:133), valor=rep(c(24000,700,25000,600,26000,500),18))
system.time(
for(i in 1:nrow(tabla)){
      cols <- tabla[i,1]
      datos[ , (cols) := lapply(.SD, "%a%", tabla[i,2]), .SDcols = cols]
})

user  system elapsed 
4.459   2.507   7.035 

system.time(lapply(1:nrow(tabla), function(i){cols <- tabla[i,1]; 
datos[ , (cols) := lapply(.SD, "%a%", tabla[i,2]), .SDcols = cols]}))

user  system elapsed 
4.351   2.295   6.654


"%p%" <- function(x, val){
      x[is.na(x)] <- val
      return(x)
}

"%a%" <- function(x, val){
      x[x > median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)] <- val
      x[x < median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE)] <- val
      return(x)
}