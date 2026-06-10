source("funcionesVarias.R")

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


comparar_algoritmos(10)