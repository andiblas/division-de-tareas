source("repartoTareas.R")

# #valoraciones = matrix(c(seq(1,17,by=2)[-9],13,1:9,seq(1,25,by=3)),9,3)
# #valoraciones=matrix(c(1:10,2:11,3:12,4:13,5:14),10,5)
# #valoraciones=matrix(c(1:5,5:1,2:6,6:2,3:7,7:3,4:8,8:4,5:9,9:5),10,5)
# #valoraciones=proporciones(valoraciones)
# n_tareas=12
# n_agentes=3
# cotizacion=rdirichlet(1,rep(1,n_tareas))
# valoraciones=t(rdirichlet(n_agentes,as.vector(cotizacion)*1000))

# reparto_0=vector(mode="list",length=dim(valoraciones)[2])
# reparto_0[[1]]=1:dim(valoraciones)[1]
# reparto_elegido_0=reparto_0
# for(i in 1:1000){
#   reparto_aux=chau_tareas_feas(valoraciones)
#   if(comparacion_leximin_pp_tareas(reparto_elegido_0,reparto_aux$reparto,valoraciones)==2){
#     reparto_elegido_0=reparto_aux$reparto
#     valoracion_obtenida_0=reparto_aux$matriz_costo_final
#   }
# }
# #reparto_elegido_0


# reparto_1=vector(mode="list",length=dim(valoraciones)[2])
# reparto_1[[1]]=1:dim(valoraciones)[1]
# reparto_elegido_1=reparto_1
# for(i in 1:1000){
#   reparto_aux=repartoTareas(n_agentes,valoraciones)
#   if(comparacion_leximin_pp_tareas(reparto_elegido_1,reparto_aux$reparto,valoraciones)==2){
#     reparto_elegido_1=reparto_aux$Art
#     valoracion_obtenida_1=reparto_aux$llevan
#   }
# }

# reparto_2=vector(mode="list",length=dim(valoraciones)[2])
# reparto_2[[1]]=1:dim(valoraciones)[1]
# reparto_elegido_2=reparto_2
# for(i in 1:1000){
#   reparto_aux=chau_tareas_feas2(valoraciones)
#   if(comparacion_leximin_pp_tareas(reparto_elegido_2,reparto_aux$reparto,valoraciones)==2){
#     reparto_elegido_2=reparto_aux$reparto
#     valoracion_obtenida_2=reparto_aux$matriz_costo_final
#   }
# }

# # diag(valoracion_obtenida_0)
# # valoracion_obtenida_1
# # diag(valoracion_obtenida_2)
# # sum(diag(valoracion_obtenida_0))
# # sum(valoracion_obtenida_1)
# # sum(diag(valoracion_obtenida_2))

# cat("\n===== Burden per agent (best of 1000 runs each) =====\n")
# cat("chau_tareas_feas : ", round(diag(valoracion_obtenida_0), 4), "\n")
# cat("repartoTareas    : ", round(valoracion_obtenida_1, 4), "\n")
# cat("chau_tareas_feas2: ", round(diag(valoracion_obtenida_2), 4), "\n")
# cat("\n===== Total burden (lower = more efficient) =====\n")
# cat("chau_tareas_feas : ", round(sum(diag(valoracion_obtenida_0)), 4), "\n")
# cat("repartoTareas    : ", round(sum(valoracion_obtenida_1), 4), "\n")
# cat("chau_tareas_feas2: ", round(sum(diag(valoracion_obtenida_2)), 4), "\n")
# cat("\n===== Max burden (lower = more fair) =====\n")
# cat("chau_tareas_feas : ", round(max(diag(valoracion_obtenida_0)), 4), "\n")
# cat("repartoTareas    : ", round(max(valoracion_obtenida_1), 4), "\n")
# cat("chau_tareas_feas2: ", round(max(diag(valoracion_obtenida_2)), 4), "\n")


# comparar_algoritmos(10)
# Uso único: encontrar el 'c' tal que esp_maximos(c, n_tareas) = 1/n_agentes.
# esp_maximos es decreciente en c (con c chico el máximo tiende a 1, con c grande
# tiende a 1/n_tareas), así que alcanza con una búsqueda binaria sobre c.
# Como es una estimación Monte Carlo, promediamos varias corridas en cada paso.
# buscar_c = function(n_tareas, n_agentes, tolerancia=0.0005, repeticiones=40,
#                     c_min=1e-4, c_max=500, max_iter=25){
#   objetivo = 1/n_agentes
#   if(objetivo <= 1/n_tareas || objetivo >= 1)
#     stop("No existe 'c': 1/n_agentes tiene que estar entre 1/n_tareas y 1")

#   estimar = function(c) mean(replicate(repeticiones, esp_maximos(c, n_tareas)))

#   for(i in 1:max_iter){
#     c_medio = sqrt(c_min*c_max)   # punto medio en escala logarítmica
#     valor = estimar(c_medio)
#     if(abs(valor-objetivo) < tolerancia) break
#     if(valor > objetivo) c_min = c_medio else c_max = c_medio
#   }
#   c_medio
# }

# # Tabla de 'c' para cada tupla (agentes, tareas) que nos interesa.
# tuplas = rbind(
#   # exhaustivos
#   data.frame(agentes=2, tareas=c(6, 9, 12, 15)),
#   data.frame(agentes=3, tareas=c(6, 7, 8, 9)),
#   data.frame(agentes=4, tareas=c(5, 6, 7, 8)),
#   # no exhaustivos
#   data.frame(agentes=2, tareas=c(20, 25, 30, 35)),
#   data.frame(agentes=3, tareas=c(15, 20, 25, 30)),
#   data.frame(agentes=4, tareas=c(10, 15, 20, 25))
# )

# tabla_c = transform(tuplas,
#   c = mapply(function(a, t) buscar_c(t, a), agentes, tareas))
# print(tabla_c, row.names=FALSE)


# a = read.table("reparto_TAREAS_6_tareas_2_agen_nrep1i_1_nrep1f_2_ninst_3_lambda_100.txt", header = TRUE)
# a$alfa_ef_mio


cantidadTareas = 6
cantidadAgentes = 2
# Perfiles de tamaños ordenados: setparts ya genera los bloques sin orden, y las
# perms de abajo les devuelven la identidad de agente, asi que con los perfiles
# canonicos alcanza para recorrer los k^n repartos exactamente una vez.
a = sum.comb(cantidadTareas, cantidadAgentes)

a
cat("-------------\n")

a = a[!apply(a, 1, is.unsorted), , drop=FALSE]

a
cat("-------------\n")

largo = dim(a)[1]
largo
cat("-------------\n")


# unclass porque perms() devuelve un objeto "partition" y sobre esa clase
# duplicated() compara elemento a elemento en vez de por columna.
permut = unclass(perms(cantidadAgentes))

permut
cat("-------------\n")

