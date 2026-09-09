source("repartoTareas.R")

# nRep = 1000
# maximos = vector(,nRep)
# n_tareas = 20
# n_agentes = 4
# alfaVec = rep(0.482087, n_tareas)
# for (i in 1:nRep) {
#   X1 = rdirichlet(1, alfaVec)
#   maximos[i] = max(X1)
# }
# mean(maximos)

LoteInicio = 1
LoteFin = 2
CantidadDeInstanciasPorCotizacion = 7
CantidadDeSegundosPorAlgoritmo = 3
Lambda = 100
Agentes = 2
Tareas = 6
simulacionTareasExhaustivo(LoteInicio, 
                LoteFin, 
                CantidadDeInstanciasPorCotizacion, 
                Lambda,
                Agentes,
                Tareas)
