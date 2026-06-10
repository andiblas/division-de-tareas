library("igraph")
library("isoband")
library("sandwich")
library("DirichletReg")
library("shiny")
library("ggplot2")
library("plotly")
library("gridExtra")
library("partitions")
library("combinat")

##############
# función envidia
##############

envidia_tareas=function(valoraciones,reparto){
  S=valoracionReparto(reparto,valoraciones)
  props=proporciones(valoraciones)
  k1=dim(S)[1]
  envidiaMat=matrix(,k1,k1)
  envyRatio=matrix(,k1,k1)
  for(i in 1:k1){
    envidiaMat[i,]=S[i,i]-S[i,]
  }
  for(i in 1:k1){
    for(j in 1:k1){
      if(S[i,j]==0){
        if(S[i,i]==0){
          envyRatio[i,j]=1  
        }else{
          envyRatio[i,j]=Inf  
        }
      }else{# cuando S[i,j]>0
        envyRatio[i,j]=S[i,i]/S[i,j]
      }
    }
  }
  maximaEnvidia=max(envidiaMat[row(envidiaMat)!=col(envidiaMat)])
  donde=which(envidiaMat == maximaEnvidia, arr.ind = TRUE)
  masEnvidioso=donde[1]
  masEnvidiado=donde[2]
  list(envidiaMat=envidiaMat,maximaEnvidia=maximaEnvidia,masEnvidioso=masEnvidioso,masEnvidiado=masEnvidiado,enviRatio=envyRatio)
}


##########
# proporciones
###########
# entrada 
# una matriz M no negativa de valoraciones M[i,j] es la valuacion del objeto i por el heredero j

# salida
# las proporciones que representa cada objeto para cada heredero

proporciones=function(M){
  n=dim(M)[1]
  k=dim(M)[2]
  Props=matrix(,n,k) # paso valores a proporciones 
  for(i in 1:k){
    Props[,i]=M[,i]/sum(M[,i])    
  }
  return(Props)
}


#entradas
# reparto es una lista, con k (cantidad de herederos) vectores 
# reparto[[i]] son los artículos que se lleva el heredero i
# proporcion es la matriz de valoración de los artículos por los herederos
# proporcion[i,j] es la proporción del total que vale el artículo i para el heredero j

# salidas
# Una matriz S de kxk donde S[i,j] es lo que siente el heredero i que se lleva j
valoracionReparto = function(reparto,valoraciones){
  props=proporciones(valoraciones)
  k=dim(props)[2]
  # for(j in 1:k){
  #   if(sum(props[,j])!=1){stop(paste("la columna",j,"de proporciones no suma 1"))}
  # }
  S=matrix(nrow=k,ncol=k)
  for(i in 1:k){
    for(j in 1:k){
      S[i,j]=sum(props[reparto[[j]],i])
    }
  }
  return(S)
}



EFX_tareas = function(reparto, valoraciones){
  props=proporciones(valoraciones)
  llevan=valoracionReparto(reparto,valoraciones)
  n=dim(valoraciones)[2]
  todos=1:n
  alfa=matrix(Inf,n,n)
  diag(alfa)=1
  efx=1
  for(i in todos){
    for(j in todos[-i]){
      if(length(reparto[[j]])==0){
        if(length(reparto[[i]])>1){
          efx=0  #en este caso alfa[i,j] sigue valiendo infinito
        }else{
          alfa[i,j]=1 # lo defino como 1 ya que no habría envidia quitandole a i la tarea que puede llegar a tener. Sería una indeterminacion 0/0
        }
      }else{  # cuando a j le reparten al menos una tarea
        if(length(reparto[[i]])>1){
          min_ii=min(props[reparto[[i]],i]) # la proporción que representa para i el trabajo más liviano que le tocó
          a=llevan[i,i]-min_ii #cuanto trabaja i según i si le quitan el trabajo más liviano.
          alfa[i,j]=a/llevan[i,j]
          if(alfa[i,j]>1){efx=0}
        }else{
          alfa[i,j]=0  # cuando i tiene a lo sumo una tarea, su ratio de EFX es 0 hacia j (quitandose esa tarea no lo envidia) y no es indeterminacion
        }
      }
    }
  }
  alfaMax=max(alfa)
  list(efx=efx,alfaMat=alfa,alfaMax=alfaMax)
}

##################
# maneras de sumar n con k números naturales
##################
sum.comb <- function(n, k) {
  
  stopifnot(k > 0L)
  
  REC <- function(n, k) {
    if (k == 1L) list(n) else
      unlist(lapply(0:n, function(i)Map(c, i, REC(n - i, k - 1L))),
             recursive = FALSE)
  }
  
  matrix(unlist(REC(n, k)), ncol = k, byrow = TRUE)
}


  

repartoExhaustivoEFX2_tareas=function(n,k,valoraciones){
  a=sum.comb(n,k)
  a=a[!apply(a,1,is.unsorted),]
  largo=dim(a)[1]
  alfaResult=Inf
  envidiaMaxima=1
  formas=0
  for(i in 1:largo){
    combinaciones=setparts(a[i,])
    cantCombi=dim(combinaciones)[2]
    permut=perms(k)
    cantPerm=dim(permut)[2]
    #cantCeros=length(which(a[i,]==0))
    #orden=k+1-rank(a[i,],ties.method = "first")
    for(j in 1:cantCombi){
      for(s in 1:cantPerm){
        repartido=vector("list", k)  
        formas=formas+1
        for(l in 1:k){
          repartido[[l]]=which(combinaciones[,j]==permut[l,s])
        }
        valoracion_aux=valoracionReparto(repartido,valoraciones) #matriz de valoracion del reparto
        alfaAux=EFX_tareas(repartido,valoraciones)$alfaMax
        envidiaMaximaAux=envidia_tareas(valoraciones,repartido)$maximaEnvidia
        if((alfaAux<=1)&(envidiaMaximaAux<envidiaMaxima)){
          repartidoResult=repartido
          alfaResult=alfaAux
          envidiaMaxima=envidiaMaximaAux
          valoracion=valoracion_aux
          #return(list(alfa=alfaResult,repartido=repartidoResult))
        }
        if(alfaAux<alfaResult){
          repartidoResult=repartido
          alfaResult=alfaAux
          envidiaMaxima=envidiaMaximaAux
          valoracion=valoracion_aux
        }
      }
    }
  }
  return(list(alfa=alfaResult,repartido=repartidoResult,envidiaMaxima=envidiaMaxima,valoracion=valoracion))
}

comparacion_leximin_pp_tareas=function(reparto1,reparto2,valoraciones){
  S1=valoracionReparto(reparto1,valoraciones)
  S2=valoracionReparto(reparto2,valoraciones)
  nAgentes=dim(S1)[1]
  valores1=diag(S1) 
  valores2=diag(S2)
  orden1=order(valores1,decreasing = TRUE)
  orden2=order(valores2,decreasing = TRUE)
  valoresOrd1=valores1[orden1]
  valoresOrd2=valores2[orden2]
  for(i in 1:nAgentes){
    if(valoresOrd1[i]<valoresOrd2[i]){
      gana=1
      return(gana)
    }  
    if(valoresOrd2[i]<valoresOrd1[i]){
      gana=2  
      return(gana)
    }
    # if(valoresOrd2[i]==valoresOrd1[i]){
    #   if(length(reparto1[[orden1[i]]])<length(reparto2[[orden2[i]]])){
    #     gana=1
    #     return(gana)
    #   }
    #   if(length(reparto2[[orden2[2]]])<length(reparto1[[orden1[i]]])){
    #     gana=2
    #     return(gana)
    #   }
    # }
  }
  # si persiste el empate, gana el orden que represnta el númer más chico
  numero1=sum(orden1*10^seq(nAgentes-1,0,by=-1))
  numero2=sum(orden2*10^seq(nAgentes-1,0,by=-1))
  if(numero1<=numero2){
    gana=1
    return(gana)}else{
      gana=2
      return(gana)}
}

########
# Falta revisar el paso1AgoritmoTareas
########

paso1AgoritmoTareas=function(reparto_orig,matriz_valoracion){   #le intentamos quitar un bien a alguno de los que sienten que se llevan más tareas a ver si mejora el leximin
  asignacionTareas=tareasAQuien(reparto_orig)
  cambio="no"
  valoran_reparto_mat=valoracionReparto(reparto_orig,matriz_valoracion)
  valoran_reparto_vec=diag(valoran_reparto_mat)
  valores_reparto=unique(valoran_reparto_vec)
  n_valores_reparto=length(valores_reparto)
  valores_reparto_ord=sort(valores_reparto) #los ordeno en orden creciente los valores del reparto de tareas
  agentes_ord=c()
  for(i in valores_reparto_ord){
    agrego=which(valoran_reparto_vec==i)
    agentes_ord=c(agentes_ord,agrego)   #ordeno los agentes de acuerdo a quien voy a intentar enchufar laburo antes
  }
  Props=proporciones(matriz_valoracion)
  n_tar=dim(Props)[1] #cantidad de tareas
  n_trab=dim(Props)[2] #cantidad de trabajadores
  
  dif=array(,c(n_trab,n_trab,n_tar)) #las diferencias e/ proporciones
  for(i in 1:n_trab){
    for(j in 1:n_trab){
      dif[i,j,]=Props[,i]-Props[,j]    
    }
  }
  #difAbs=abs(dif)
  for(i in agentes_ord){    # le voy a intentar enchufar tareas a los que menos laburan primero
    if(length(reparto_orig[[i]])==0){tareasAEnchufar=1:n_tar}else{
      tareasAEnchufar=(1:n_tar)[-reparto_orig[[i]]]   #son todas las tareas que no le tocaron al agente al que le quiero enchufar una tarea  
    }
    
    n_tareasAEnchufar=length(tareasAEnchufar)
    diferencias=vector(,length=n_tareasAEnchufar)
    for(j in 1:n_tareasAEnchufar){
      diferencias[j]=dif[i,asignacionTareas[tareasAEnchufar[j]],tareasAEnchufar[j]]
    }
    ordenDiferencias=order(diferencias) # es en el orden que voy a intentar enchufarle las tareas
      
    for(k in  ordenDiferencias){
        reparto_nuevo=entregaBien(reparto_orig,i,asignacionTareas[tareasAEnchufar[k]],tareasAEnchufar[k])  
        if(comparacion_leximin_pp_tareas(reparto_orig,reparto_nuevo,matriz_valoracion)==2){
          cambio="si"
          return(list(reparto_nuevo=reparto_nuevo,cambio=cambio))
        }
    }
  }
  return(list(reparto_nuevo=reparto_orig,cambio=cambio))
}
  

  
repartoTareas=function(n_trab,matriz_valoracion){
  M=matriz_valoracion
  n_tareas=dim(M)[1]
  if(n_trab != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de trabajadores")}
  
  Props=proporciones(M)
  
  art=list()
  art[[n_trab+1]]=1
  art[[n_trab+1]]=c()
  for(i in 1:n_tareas){
    #maximizan=which(Props[i,]==max(Props[i,]))
    #n_max=length(maximizan)
    #j=maximizan[sample(n_max)]
    j=sample(n_trab,1)
    art[[j]]=c(art[[j]],i)
  }
  
  reparto_orig=art
  cambio="si"
  ss=1
  while(cambio=="si"){
    ss=ss+1
    repartoAux=paso1AgoritmoTareas(reparto_orig,M)
    reparto_orig=repartoAux$reparto_nuevo
    cambio=repartoAux$cambio
  }
  ###################################
  # Falta agregar el paso2Algoritmo #
  ###################################
  
  lleva=diag(valoracionReparto(reparto_orig,matriz_valoracion))
  
  return(list(Art=reparto_orig,llevan=lleva))
}


#* Calculate chore allocation using the Top-Trading Envy-Cycle Elimination algorithm
#* (Bhaskar, Sricharan, Vaish 2022 — Algorithm 2)
#*
#* Guarantees an EF1 (envy-free up to one chore) allocation for additive valuations.
#* Iterates through chores one at a time. Each chore is assigned to a "sink" agent
#* (one who does not envy anyone). If no sink exists, resolves a cycle in the
#* top-trading envy graph (where each agent points to their most preferred bundle)
#* to create one.
#*
#* @param n_trab Integer, number of agents
#* @param matriz_valoracion Matrix[n_tareas x n_trab] of dislike costs (positive values, higher = more disliked)
#* @return list(Art, llevan) — Art[[i]] = chore indices for agent i, llevan[i] = normalized burden
repartoTareasTopTrading=function(n_trab, matriz_valoracion){
  M=matriz_valoracion
  n_tareas=dim(M)[1]
  if(n_trab != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de trabajadores")}

  Art=vector("list", n_trab)
  for(i in seq_len(n_trab)) Art[[i]]=integer(0)

  # Cost of a bundle of chores for a given agent (additive)
  costo_paquete=function(agente, tareas){
    if(length(tareas)==0) return(0)
    sum(M[tareas, agente])
  }

  # Find a sink in the envy graph: an agent who does not envy anyone.
  # For chores, agent i envies agent k if cost_i(A_k) < cost_i(A_i).
  # A sink has no outgoing edges — their bundle is at most as costly as any
  # other bundle from their own perspective.
  encontrar_sink=function(alloc){
    sinks=c()
    for(i in seq_len(n_trab)){
      mi_costo=costo_paquete(i, alloc[[i]])
      es_sink=TRUE
      for(k in seq_len(n_trab)){
        if(k==i) next
        if(costo_paquete(i, alloc[[k]]) < mi_costo){
          es_sink=FALSE
          break
        }
      }
      if(es_sink) sinks=c(sinks, i)
    }
    if(length(sinks)==0) return(NULL)
    sinks[sample.int(length(sinks), 1)]
  }

  # Build the top-trading envy graph and find a cycle.
  # Each envious agent points to the agent whose bundle they prefer most
  # (lowest cost). Agents who are sinks have no outgoing edge.
  # By Lemma 6 of the paper, if G_A has no sink then T_A must have a cycle.
  encontrar_ciclo_top_trading=function(alloc){
    puntero=integer(n_trab)
    for(i in seq_len(n_trab)){
      mi_costo=costo_paquete(i, alloc[[i]])
      mejor_agente=0L
      mejor_costo=mi_costo
      for(k in seq_len(n_trab)){
        if(k==i) next
        k_costo=costo_paquete(i, alloc[[k]])
        if(k_costo < mejor_costo){
          mejor_costo=k_costo
          mejor_agente=k
        }
      }
      puntero[i]=mejor_agente
    }

    visitado=integer(n_trab)
    paso=0L
    for(inicio in seq_len(n_trab)){
      if(visitado[inicio]>0) next
      camino=c()
      actual=inicio
      while(actual>0 && visitado[actual]==0){
        paso=paso+1L
        visitado[actual]=paso
        camino=c(camino, actual)
        actual=puntero[actual]
      }
      if(actual>0 && actual %in% camino){
        idx_inicio=which(camino==actual)
        return(camino[idx_inicio:length(camino)])
      }
    }
    return(NULL)
  }

  # Resolve a cycle: each agent in the cycle receives the bundle of the agent
  # they point to (swap backwards along the cycle). Agents outside the cycle
  # keep their bundles. After resolution, all cycle participants hold their
  # most preferred bundle and become sinks (Lemma 7).
  resolver_ciclo=function(alloc, ciclo){
    n_ciclo=length(ciclo)
    paquetes_guardados=lapply(ciclo, function(i) alloc[[i]])
    for(idx in seq_along(ciclo)){
      sig_idx=if(idx==n_ciclo) 1L else idx+1L
      alloc[[ciclo[idx]]]=paquetes_guardados[[sig_idx]]
    }
    return(alloc)
  }

  # Main loop: assign each chore to a sink agent (Algorithm 2)
  for(c in seq_len(n_tareas)){
    sink_agente=encontrar_sink(Art)

    if(is.null(sink_agente)){
      ciclo=encontrar_ciclo_top_trading(Art)
      if(!is.null(ciclo)){
        Art=resolver_ciclo(Art, ciclo)
      }
      sink_agente=encontrar_sink(Art)
    }

    Art[[sink_agente]]=c(Art[[sink_agente]], c)
  }

  lleva=diag(valoracionReparto(Art, matriz_valoracion))

  return(list(Art=Art, llevan=lleva))
}


#* Calculate chore allocation using the Round Robin method
#* @param dislikeMatrix A matrix where rows represent chores, columns represent agents, and values represent the dislike scores of agents for chores
#* @param agentsOrder Integer vector of agent indices giving the order in which agents pick
repartoTareasRoundRobin=function(dislikeMatrix, agentsOrder) {
  agentsCount <- ncol(dislikeMatrix)
  n_chores <- nrow(dislikeMatrix)

  # One empty slot per agent to accumulate assigned chore indices
  art <- vector("list", agentsCount)
  for (i in seq_len(agentsCount)) art[[i]] <- integer(0)

  # Track which chores haven't been picked yet
  available <- seq_len(n_chores)

  while (length(available) > 0) {
    for (agent in agentsOrder) {
      if (length(available) == 0) break
      # Each agent picks the available chore they dislike the least
      agent_dislikes <- dislikeMatrix[available, agent]
      pick_pos <- which.min(agent_dislikes)
      chosen_chore <- available[pick_pos]
      art[[agent]] <- c(art[[agent]], chosen_chore)
      # Remove picked chore so no other agent can take it
      available <- available[-pick_pos]
    }
  }

  # Compute each agent's burden as their share of their own valuation
  lleva <- diag(valoracionReparto(art, dislikeMatrix))

  return(list(Art = art, llevan = lleva))
}

#* Run repartoTareasRoundRobin for every possible agent ordering.
#* @param dislikeMatrix A matrix where rows represent chores, columns represent agents, and values represent the dislike scores of agents for chores
#* Returns a list with one entry per permutation, each a list(order, Art, llevan).
repartoTareasAllRoundRobins=function(dislikeMatrix) {
  agentsCount <- ncol(dislikeMatrix)
  orderings <- permn(agentsCount)

  results <- vector("list", length(orderings))
  for (k in seq_along(orderings)) {
    ord <- orderings[[k]]
    res <- repartoTareasRoundRobin(dislikeMatrix, ord)
    results[[k]] <- list(order = ord, Art = res$Art, llevan = res$llevan)
  }

  results
}

tareasAQuien=function(reparto){
  n_trab=length(reparto)
  n_tareas=sum(lengths(reparto))
  asignoTareas=vector(,n_tareas)
  for(i in 1:n_trab){
    asignoTareas[reparto[[i]]]=i
  }
  asignoTareas
}

chau_tareas_feas = function(valoraciones){
  M=valoraciones
  n_tareas=dim(M)[1]
  n_agentes=dim(M)[2]
  M=proporciones(M) #para que las columnas sumen 1
  reparto = vector(mode="list",length=n_agentes)
  restantes=1:n_tareas
  while(length(restantes)>0){
    matriz_costo=valoracionReparto(reparto,M)  #cuánto sienten que trabajan con lo repartido hasta aquí
    i_star = which(diag(matriz_costo)==max(diag(matriz_costo))) #quienes son los que más sienten que trabajan
    largo_i_star=length(i_star)
    sorteo_i=sample(1:largo_i_star,1)
    valoracion_restantes = M[restantes,i_star[sorteo_i]] # vemos como valora lo que queda el que va a elegir una tarea para que le asignen a otro
    c_star = which(valoracion_restantes==max(valoracion_restantes)) # las tareas más pesadas
    largo_c_star=length(c_star)
    sorteo_c=sample(1:largo_c_star,1)
    postulantes=(1:n_agentes)[-i_star[sorteo_i]] # los que pueden recibir la tarea (los que menos trabajn hasta aquí)
    j_star_indices = which(diag(matriz_costo)[-i_star[sorteo_i]]==min(diag(matriz_costo)[-i_star[sorteo_i]]))
    largo_j_star_indices=length(j_star_indices)
    sorteo_j=sample(1:largo_j_star_indices,1)
    j_star=postulantes[j_star_indices[sorteo_j]]
    reparto[[j_star]]=c(reparto[[j_star]],restantes[c_star[sorteo_c]])
    restantes = setdiff(restantes,restantes[c_star[sorteo_c]])
  }
  matriz_costo_final=valoracionReparto(reparto,M)
  list(reparto=reparto,matriz_costo_final=matriz_costo_final)
}

chau_tareas_feas2 = function(valoraciones){
  M=valoraciones
  n_tareas=dim(M)[1]
  n_agentes=dim(M)[2]
  M=proporciones(M)

  sorteo=function(candidatos){
    candidatos[sample.int(length(candidatos),1)]
  }

  agente_mas_cargado=function(reparto){
    costos=diag(valoracionReparto(reparto,M))
    sorteo(which(costos==max(costos)))
  }

  tarea_mas_pesada=function(restantes, agente){
    valoracion_restantes=M[restantes,agente]
    restantes[sorteo(which(valoracion_restantes==max(valoracion_restantes)))]
  }

  agente_menor_costo=function(tarea, excluido){
    postulantes=(1:n_agentes)[-excluido]
    costos_postulantes=M[tarea,postulantes]
    postulantes[sorteo(which(costos_postulantes==min(costos_postulantes)))]
  }

  reparto = vector(mode="list",length=n_agentes)
  restantes=1:n_tareas
  while(length(restantes)>0){
    i_star=agente_mas_cargado(reparto)
    tarea_elegida=tarea_mas_pesada(restantes, i_star)
    j_star=agente_menor_costo(tarea_elegida, i_star)
    reparto[[j_star]]=c(reparto[[j_star]],tarea_elegida)
    restantes=setdiff(restantes,tarea_elegida)
  }
  matriz_costo_final=valoracionReparto(reparto,M)
  list(reparto=reparto,matriz_costo_final=matriz_costo_final)
}


comparar_algoritmos = function(n_tests, n_tareas=12, n_agentes=3, n_iter=1000){
  nombres = c("chau_tareas_feas", "repartoTareas", "chau_tareas_feas2")
  victorias = setNames(integer(3), nombres)
  totales = matrix(nrow=n_tests, ncol=3, dimnames=list(NULL, nombres))

  reparto_inicial = function(valoraciones){
    reparto = vector(mode="list", length=dim(valoraciones)[2])
    reparto[[1]] = 1:dim(valoraciones)[1]
    reparto
  }

  mejor_de_n = function(algoritmo, valoraciones, n_iter){
    reparto_elegido = reparto_inicial(valoraciones)
    mejor_carga = Inf
    for(i in 1:n_iter){
      reparto_aux = algoritmo(valoraciones)
      if(comparacion_leximin_pp_tareas(reparto_elegido, reparto_aux$reparto, valoraciones)==2){
        reparto_elegido = reparto_aux$reparto
        mejor_carga = reparto_aux$carga_total
      }
    }
    mejor_carga
  }

  wrapper_0 = function(valoraciones){
    res = chau_tareas_feas(valoraciones)
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_1 = function(valoraciones){
    res = repartoTareas(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    # res$alpha minimizarlo
    res
  }
  wrapper_2 = function(valoraciones){
    res = chau_tareas_feas2(valoraciones)
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }

  wrappers = list(wrapper_0, wrapper_1, wrapper_2)

  for(t in 1:n_tests){
    cotizacion = rdirichlet(1, rep(1, n_tareas))
    valoraciones = t(rdirichlet(n_agentes, as.vector(cotizacion)*1000))

    for(a in 1:3){
      totales[t, a] = mejor_de_n(wrappers[[a]], valoraciones, n_iter)
    }

    ganador = which(totales[t, ]==min(totales[t, ]))
    victorias[ganador] = victorias[ganador] + 1

    cat(sprintf("Test %d/%d — burdens: %.4f | %.4f | %.4f — winner: %s\n",
                t, n_tests, totales[t,1], totales[t,2], totales[t,3], nombres[ganador]))
  }

  cat("\n===== Results =====\n")
  for(a in 1:3){
    cat(sprintf("%-20s  wins: %d/%d (%.1f%%)  avg burden: %.4f\n",
                nombres[a], victorias[a], n_tests,
                100*victorias[a]/n_tests, mean(totales[,a])))
  }

  list(victorias=victorias, totales=totales)
}


# desarollar envidiaParaTareas asi calculamos el alpha de cada asignación
# incorporar toptradingEnvyCycle a la simulación
# agregar el calculo de los alphas en cada algoritmo
# quedarse con el menor alpha. puede ser que el de LiptonTopTrading de mejor alpha.
# chau_tareas_feas2 modificaciones: hacer una variante con verficiación del mejor vector de satisfacción. leer mail con otras mejoras
