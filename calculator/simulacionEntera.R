source("repartoTareas.R")

# PRUEBA 1: corre los algoritmos no exhaustivos sobre el mismo universo de
# (agentes, tareas) con el que Agustin corrio el exhaustivo, para poder
# contrastar. Son 3 lambdas x 12 tuplas = 36 corridas.
#
# Duracion aproximada por corrida. simulacionTareas paraleliza las filas entre
# detectCores()-1 workers, asi que la cuenta va por chunks de filas y no por fila:
#   filas   = (LoteFin - LoteInicio + 1) * CantidadDeInstanciasPorCotizacion
#   chunks  = ceiling(filas / (detectCores() - 1))
#   duracion ~ chunks * 8 * CantidadDeSegundosPorAlgoritmo segundos
# (la formula serial de repartoTareas.R no divide por los workers: esa es la cota
# de arriba, no lo que tarda.) Los dos algoritmos deterministas no suman tiempo
# apreciable ni siquiera en la tupla mas grande, asi que el costo es parejo entre
# tuplas. Medido en una M1 Pro (8 cores -> 7 workers) con los valores de abajo:
# 100 filas / 7 = 15 chunks * 24s = ~6 min por corrida, ~3.6 hs el lote completo.
# Conviene largarlo con nohup/tmux y redirigir la salida a un archivo de log.
#
# Cada corrida escribe sus propios archivos reparto_TAREAS_*.txt e
# instancia_TAREAS_*.txt: el sufijo incluye tareas, agentes, lote, instancias y
# lambda, asi que las 36 no se pisan entre si.

# Parametros compartidos por las 36 corridas
LoteInicio = 1
LoteFin = 10
CantidadDeInstanciasPorCotizacion = 10
CantidadDeSegundosPorAlgoritmo = 3

lambdas = c(10, 100, 1000)

combinaciones = data.frame(
  agentes = c(2, 2,  2,  2, 3, 3, 3, 3, 4, 4, 4, 4),
  tareas  = c(6, 9, 12, 15, 6, 7, 8, 9, 5, 6, 7, 8)
)

total_corridas = length(lambdas) * nrow(combinaciones)
fallidas = data.frame(lambda=numeric(0), agentes=numeric(0), tareas=numeric(0), error=character(0))
corrida = 0

for(Lambda in lambdas){
  for(k in seq_len(nrow(combinaciones))){
    Agentes = combinaciones$agentes[k]
    Tareas  = combinaciones$tareas[k]
    corrida = corrida + 1

    cat(sprintf("\n########## corrida %d/%d — lambda=%s agentes=%d tareas=%d ##########\n",
                corrida, total_corridas, format(Lambda), Agentes, Tareas))

    # Un error en una corrida no tiene que tirar abajo las horas que faltan:
    # lo anotamos, seguimos, y lo listamos al final.
    tryCatch(
      simulacionTareas(LoteInicio,
                      LoteFin,
                      CantidadDeInstanciasPorCotizacion,
                      CantidadDeSegundosPorAlgoritmo,
                      Lambda,
                      Agentes,
                      Tareas),
      error = function(e){
        cat(sprintf("FALLO lambda=%s agentes=%d tareas=%d: %s\n",
                    format(Lambda), Agentes, Tareas, conditionMessage(e)))
        fallidas[nrow(fallidas)+1, ] <<- list(Lambda, Agentes, Tareas, conditionMessage(e))
      }
    )
  }
}

if(nrow(fallidas) == 0){
  cat(sprintf("\nLas %d corridas terminaron sin errores.\n", total_corridas))
} else {
  cat(sprintf("\n===== Corridas fallidas (%d/%d) =====\n", nrow(fallidas), total_corridas))
  for(k in seq_len(nrow(fallidas))){
    cat(sprintf("lambda=%s agentes=%d tareas=%d: %s\n",
                format(fallidas$lambda[k]), fallidas$agentes[k], fallidas$tareas[k], fallidas$error[k]))
  }
}
