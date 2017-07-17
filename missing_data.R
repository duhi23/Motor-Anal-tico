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

system.time(var_per <- which(unlist(lapply(datos[,vars, with=FALSE], perdido))))
# var_per deberia ser un data.frame que a mas de contener el numero de la columna
# que presenta valores perdidos, debe poseer el valor a reemplazar
for(i in 1:length(var_per)){
      cols <- tabla[i,1]
      datos[ , (cols) := lapply(.SD, "%p%", tabla[i,2]), .SDcols = cols]
}


system.time(var_atp <- which(unlist(lapply(datos[,vars, with=FALSE], atipico))))
# var_atp deberia ser un data.frame que a mas de contener el numero de la columna que
# presenta valores atipicos, debe poseer el valor a reemplazar o a su vez eliminar

# var_num[as.vector(var_atp)] # posiciones de las variables en la base original
# tabla <- data.frame(col=c(14:37,50:133), valor=rep(c(24000,700,25000,600,26000,500),18))
system.time(
for(i in 1:nrow(tabla)){
      cols <- tabla[i,1]
      datos[ , (cols) := lapply(.SD, "%a%", tabla[i,2]), .SDcols = cols]
})


system.time(lapply(1:nrow(tabla), function(i){cols <- tabla[i,1]; 
datos[ , (cols) := lapply(.SD, "%a%", tabla[i,2]), .SDcols = cols]}))


"%p%" <- function(x, val){
      x[is.na(x)] <- val
      return(x)
}

"%a%" <- function(x, val){
      x[x > median(x, na.rm = TRUE) + 1.5*IQR(x, na.rm = TRUE)] <- val
      x[x < median(x, na.rm = TRUE) - 1.5*IQR(x, na.rm = TRUE)] <- val
      return(x)
}




datos <- data.table(read_tsv("TemplateCobranzaTotal.txt"))
vars <- colnames(datos)[as.vector(which(unlist(lapply(datos, class)) == "numeric"))]
var_num <- as.vector(which(unlist(lapply(datos, class)) == "numeric"))

# Función is.atipico 
# data: datos en formato data.table sin valores perdidos
# vars: vector de variables numéricas
# retorna vector de nombres de variables con valores atipicos
is.atipico <- function(data, vars){
      atipico <- function(x){
            val <- any(x > median(x) + 1.5*IQR(x)) | any(x < median(x) - 1.5*IQR(x))
            return(val)
      }
      ls_atp <- vars[as.vector(which(unlist(lapply(data[,vars, with=FALSE], atipico))))]
      return(ls_atp)
}

vars_atp <- is.atipico(datos, vars)

tabla <- data.frame(variable=c("SALDO_NOR_M1", "SALDO_NOR_M5", "SALDO_NOR_M10", "SALDO_NOR_M11"), 
                    accion=c("eliminar", "reemplazar", "eliminar", "reemplazar"), 
                    stringsAsFactors = FALSE)


# Función atipico
# data: datos en formato data.table sin valores perdidos
# criterio: tabla con las acciones a tomar sobre las variables
# vars: vector de variables con valores atipicos
# 
atipico <- function(data, criterio, vars){
      sub_cri <- criterio[criterio[,2]=="eliminar",]
      nvars <- nrow(sub_cri)
      if(nvars>0){
            cols_dlt <- c()
            for(i in 1:nvars){
                  cols_dlt <- c(grep(sub_cri[[1]][i], vars)[1], cols_dlt)
            }
      }
}

