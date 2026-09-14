source("repartoTareas.R")

# Parametros compartidos por las 36 corridas
LoteInicio = 1
LoteFin = 10
CantidadDeInstanciasPorCotizacion = 10

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
      simulacionTareasExhaustivo(LoteInicio,
                      LoteFin,
                      CantidadDeInstanciasPorCotizacion,
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
