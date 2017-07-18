library(data.table)
library(haven)
library(readr)


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
# retorna los datos eliminados o corregidos en base al criterio ingresado
atipico <- function(data, criterio, vars){
      sub_cri <- criterio[criterio[,2]=="eliminar",]
      nvars <- nrow(sub_cri)
      if(nvars>0){
            ind_atp <- function(data){
                  numcol <- ncol(data)
                  vec <- c()
                  for(i in 1:numcol){
                        valsup <- median(data[[i]]) + 1.5*IQR(data[[i]])
                        valinf <- median(data[[i]]) - 1.5*IQR(data[[i]])
                        ind <- which(data[[i]] > valsup | data[[i]] < valinf)
                        vec <- c(vec, ind)
                  }
                  return(unique(vec))
            }
            data <- data[-ind_atp(data[,sub_cri[[1]], with=FALSE]),]
      }
      atp <- function(x){
            valsup <- median(x) + 1.5*IQR(x)
            valinf <- median(x) - 1.5*IQR(x)
            x[x > median(x) + 1.5*IQR(x)] <- valsup
            x[x < median(x) - 1.5*IQR(x)] <- valinf
            return(x)
      }
            lapply(1:length(vars), function(i){cols <- vars[i]; 
            data[ , (cols) := lapply(.SD, atp), .SDcols = cols]})
      return(data)
}



