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
library("parallel")

entregaBien=function(reparto_orig,benef_recibe,benef_entrega,bien_entrega){
  if(!(bien_entrega %in% reparto_orig[[benef_entrega]])){stop("no puede entregar ese bien ese beneficiario")}
  reparto_nuevo=reparto_orig
  reparto_nuevo[[benef_recibe]]=c(reparto_orig[[benef_recibe]],bien_entrega)
  reparto_nuevo[[benef_entrega]]=setdiff(reparto_orig[[benef_entrega]],bien_entrega)
  reparto_nuevo
}

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

envidia2=function(valoraciones,reparto){
  S=valoracionReparto(reparto,valoraciones)
  props=proporciones(valoraciones)
  n_agentes=dim(valoraciones)[2]
  alfa_ef_mat=alfa_efx_mat=alfa_ef1_mat=matrix(,n_agentes,n_agentes)
  alfa_prop_vec=alfa_propx_vec=alfa_prop1_vec=vector(,n_agentes)
  diag(alfa_ef_mat)=diag(alfa_efx_mat)=diag(alfa_ef1_mat)=1
  #envidiaMat=envyRatioMat=matrix(,k1,k1)
  #envidian=vector(,length=k1)
  for(i in 1:n_agentes){
    for(j in (1:n_agentes)[-i]){
      if((S[i,i]==0)&(S[i,j]==0)){
        alfa_ef_mat[i,j]=1
      }else{
        alfa_ef_mat[i,j]=S[i,i]/S[i,j]
      }
      #ahora alfa_efx_mat
      if(length(reparto[[j]])==0) {
        denom1_ij=0
      }
      else {
        denom1_ij=S[i,j]-min(props[reparto[[j]],i])
      }
      if((S[i,i]==0)&(denom1_ij==0)){
        alfa_efx_mat[i,j]=1
      }else{
        alfa_efx_mat[i,j]=S[i,i]/denom1_ij
      }
      
      #ahora alfa_ef1_mat
      if(length(reparto[[j]])==0){
        denom2_ij=0}else{
          denom2_ij=S[i,j]-max(props[reparto[[j]],i])
        }
      if((S[i,i]==0)&(denom2_ij==0)){
        alfa_ef1_mat[i,j]=1
      }else{
        alfa_ef1_mat[i,j]=S[i,i]/denom2_ij
      }
    }
  }
  alfa_ef=min(alfa_ef_mat[row(alfa_ef_mat)!=col(alfa_ef_mat)])
  alfa_efx=min(alfa_efx_mat[row(alfa_efx_mat)!=col(alfa_efx_mat)])
  alfa_ef1=min(alfa_ef1_mat[row(alfa_ef1_mat)!=col(alfa_ef1_mat)])
  
  alfa_prop=min(n_agentes*diag(S))
  for(i in 1:n_agentes){
    alfa_prop1_vec[i]=(S[i,i]+max(props[-reparto[[i]],i]))*n_agentes
    alfa_propx_vec[i]=(S[i,i]+min(props[-reparto[[i]],i]))*n_agentes
  }
  alfa_prop1=min(alfa_prop1_vec)
  alfa_propx=min(alfa_propx_vec)
  list(alfa_ef=alfa_ef,
       alfa_ef1=alfa_ef1,
       alfa_efx=alfa_efx,
       alfa_prop=alfa_prop,
       alfa_prop1=alfa_prop1,
       alfa_propx=alfa_propx)
}

envidia2_tareas = function(valoraciones, reparto) {
  S = valoracionReparto(reparto, valoraciones)
  props = proporciones(valoraciones)
  n_agentes = dim(valoraciones)[2]

  alfa_ef_mat = alfa_efx_mat = alfa_ef1_mat = matrix(, n_agentes, n_agentes)
  alfa_prop1_vec = alfa_propx_vec = vector(, n_agentes)
  diag(alfa_ef_mat) = diag(alfa_efx_mat) = diag(alfa_ef1_mat) = 1

  for(i in 1:n_agentes) {
    for(j in (1:n_agentes)[-i]) {

      # Base EF: same ratio as goods but α > 1 now signals envy
      if((S[i,i] == 0) & (S[i,j] == 0)) {
        alfa_ef_mat[i,j] = 1
      } else {
        alfa_ef_mat[i,j] = S[i,i] / S[i,j]   # Inf when S[i,j]=0 and S[i,i]>0
      }

      # EFX: remove the *lightest* chore from i's bundle (hardest condition — worst-case removal)
      if(length(reparto[[i]]) == 0) {
        num_efx = 0
      } else {
        num_efx = S[i,i] - min(props[reparto[[i]], i])
      }
      if((num_efx == 0) & (S[i,j] == 0)) {
        alfa_efx_mat[i,j] = 1
      } else {
        alfa_efx_mat[i,j] = num_efx / S[i,j]
      }

      # EF1: remove the *heaviest* chore from i's bundle (easiest condition — best-case removal)
      if(length(reparto[[i]]) == 0) {
        num_ef1 = 0
      } else {
        num_ef1 = S[i,i] - max(props[reparto[[i]], i])
      }
      if((num_ef1 == 0) & (S[i,j] == 0)) {
        alfa_ef1_mat[i,j] = 1
      } else {
        alfa_ef1_mat[i,j] = num_ef1 / S[i,j]
      }
    }
  }

  # Worst-case is now the maximum (α > 1 is bad for chores)
  alfa_ef  = max(alfa_ef_mat[row(alfa_ef_mat) != col(alfa_ef_mat)])
  alfa_efx = max(alfa_efx_mat[row(alfa_efx_mat) != col(alfa_efx_mat)])
  alfa_ef1 = max(alfa_ef1_mat[row(alfa_ef1_mat) != col(alfa_ef1_mat)])

  # Proportionality: want n*S[i,i] ≤ 1, so max is the relevant aggregate
  alfa_prop = max(n_agentes * diag(S))

  for(i in 1:n_agentes) {
    if(length(reparto[[i]]) == 0) {
      alfa_prop1_vec[i] = 0
      alfa_propx_vec[i] = 0
    } else {
      # PROP1: remove heaviest chore from i (easiest to satisfy)
      alfa_prop1_vec[i] = (S[i,i] - max(props[reparto[[i]], i])) * n_agentes
      # PROPx: remove lightest chore from i (hardest to satisfy)
      alfa_propx_vec[i] = (S[i,i] - min(props[reparto[[i]], i])) * n_agentes
    }
  }
  alfa_prop1 = max(alfa_prop1_vec)
  alfa_propx = max(alfa_propx_vec)

  bienestar_nash = productoria(1-diag(S))$producto

  list(
    alfa_ef    = alfa_ef,
    alfa_ef1   = alfa_ef1,
    alfa_efx   = alfa_efx,
    alfa_prop  = alfa_prop,
    alfa_prop1 = alfa_prop1,
    alfa_propx = alfa_propx,
    bienestar_nash = bienestar_nash
  )
}

productoria = function(vector){
  prod=prodSinCero=1
  for(i in 1:length(vector)){
    prod=prod*vector[i]
    if(vector[i]!=0){
      prodSinCero=prodSinCero*vector[i]
    }
  }
  list(producto=prod,productoSinCeros=prodSinCero)
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
  

# RepartoTareas:
#
# Primer paso: reparto al azar las tareas entre los agentes.
# Se le intenta dar al que menos siente que labura una tarea.
# Se busca en orden entre las tareas que no le tocaron empezando por aquella que a él le cuesta muy poco en relación al que
# le tocó en el reparto original y así siguiendo con el resto de las tareas.
# Si en alguno de esos intentos se logra mejorar el leximin entonces se hace la entrega de la tarea y cambia el reparto original
# y se vuelve a empezar.
# Si no se le logra dar al que menos sentía que labura a se intenta con el segundo, y así siguiendo. Si no se le logra dar tarea a ninguno mejorando el leximin se termina.
# Habría que programar un análogo donde no se mira lo del leximin sino el coeficiente de envyratio. Eso no lo hice.

  
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

# Top-Trading envy-cycle helpers (Bhaskar, Sricharan, Vaish 2022 — Algorithm 2)

# Cost of a bundle of chores for a given agent (additive)
top_trading_costo_paquete=function(agente, tareas, M){
  if(length(tareas)==0) return(0)
  sum(M[tareas, agente])
}

# Find a sink in the envy graph: an agent who does not envy anyone.
# For chores, agent i envies agent k if cost_i(A_k) < cost_i(A_i).
# A sink has no outgoing edges — their bundle is at most as costly as any
# other bundle from their own perspective.
top_trading_encontrar_sink=function(alloc, n_trab, M){
  sinks=c()
  for(i in seq_len(n_trab)){
    mi_costo=top_trading_costo_paquete(i, alloc[[i]], M)
    es_sink=TRUE
    for(k in seq_len(n_trab)){
      if(k==i) next
      if(top_trading_costo_paquete(i, alloc[[k]], M) < mi_costo){
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
top_trading_encontrar_ciclo=function(alloc, n_trab, M){
  puntero=integer(n_trab)
  for(i in seq_len(n_trab)){
    mi_costo=top_trading_costo_paquete(i, alloc[[i]], M)
    mejor_agente=0L
    mejor_costo=mi_costo
    for(k in seq_len(n_trab)){
      if(k==i) next
      k_costo=top_trading_costo_paquete(i, alloc[[k]], M)
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
top_trading_resolver_ciclo=function(alloc, ciclo){
  n_ciclo=length(ciclo)
  paquetes_guardados=lapply(ciclo, function(i) alloc[[i]])
  for(idx in seq_along(ciclo)){
    sig_idx=if(idx==n_ciclo) 1L else idx+1L
    alloc[[ciclo[idx]]]=paquetes_guardados[[sig_idx]]
  }
  return(alloc)
}

# Resolve a top-trading envy cycle if no sink exists, then return a sink agent.
top_trading_resolver_sin_sink=function(alloc, n_trab, M){
  sink_agente=top_trading_encontrar_sink(alloc, n_trab, M)
  if(is.null(sink_agente)){
    ciclo=top_trading_encontrar_ciclo(alloc, n_trab, M)
    if(!is.null(ciclo)){
      alloc=top_trading_resolver_ciclo(alloc, ciclo)
    }
    sink_agente=top_trading_encontrar_sink(alloc, n_trab, M)
  }
  list(alloc=alloc, sink_agente=sink_agente)
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

  for(c in seq_len(n_tareas)){
    res=top_trading_resolver_sin_sink(Art, n_trab, M)
    Art=res$alloc
    Art[[res$sink_agente]]=c(Art[[res$sink_agente]], c)
  }

  lleva=diag(valoracionReparto(Art, matriz_valoracion))

  return(list(Art=Art, llevan=lleva))
}

#* Same as repartoTareasTopTrading, but picks the next chore uniformly at random
#* from the remaining chores on each iteration instead of a fixed sequential order.
#*
#* @param n_trab Integer, number of agents
#* @param matriz_valoracion Matrix[n_tareas x n_trab] of dislike costs (positive values, higher = more disliked)
#* @return list(Art, llevan) — Art[[i]] = chore indices for agent i, llevan[i] = normalized burden
repartoTareasTopTradingRandom=function(n_trab, matriz_valoracion){
  M=matriz_valoracion
  n_tareas=dim(M)[1]
  if(n_trab != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de trabajadores")}

  Art=vector("list", n_trab)
  for(i in seq_len(n_trab)) Art[[i]]=integer(0)

  restantes=seq_len(n_tareas)
  while(length(restantes)>0){
    res=top_trading_resolver_sin_sink(Art, n_trab, M)
    Art=res$alloc

    pick_pos=sample.int(length(restantes), 1)
    c=restantes[pick_pos]
    Art[[res$sink_agente]]=c(Art[[res$sink_agente]], c)
    restantes=restantes[-pick_pos]
  }

  lleva=diag(valoracionReparto(Art, matriz_valoracion))

  return(list(Art=Art, llevan=lleva))
}

#* Same as repartoTareasTopTradingRandom, but resolves one top-trading envy cycle
#* after the last chore has been assigned.
#*
#* @param n_trab Integer, number of agents
#* @param matriz_valoracion Matrix[n_tareas x n_trab] of dislike costs (positive values, higher = more disliked)
#* @return list(Art, llevan) — Art[[i]] = chore indices for agent i, llevan[i] = normalized burden
repartoTareasTopTradingRandomLastEnvyCycle=function(n_trab, matriz_valoracion){
  M=matriz_valoracion
  n_tareas=dim(M)[1]
  if(n_trab != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de trabajadores")}

  Art=vector("list", n_trab)
  for(i in seq_len(n_trab)) Art[[i]]=integer(0)

  restantes=seq_len(n_tareas)
  while(length(restantes)>0){
    res=top_trading_resolver_sin_sink(Art, n_trab, M)
    Art=res$alloc

    pick_pos=sample.int(length(restantes), 1)
    c=restantes[pick_pos]
    Art[[res$sink_agente]]=c(Art[[res$sink_agente]], c)
    restantes=restantes[-pick_pos]
  }

  res=top_trading_resolver_sin_sink(Art, n_trab, M)
  Art=res$alloc

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

chauTareasFeas2 = function(valoraciones){
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

# chauTareasFeas2Random es la versión de ChauTareasFeas2
# en la cual el criterio de selección del agente a la que se
# le asigna la tarea es al azar
chauTareasFeas2Random = function(valoraciones){
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

  reparto = vector(mode="list",length=n_agentes)
  restantes=1:n_tareas
  while(length(restantes)>0){
    i_star=agente_mas_cargado(reparto)
    tarea_elegida=tarea_mas_pesada(restantes, i_star)
    j_star=sorteo((1:n_agentes)[-i_star])
    reparto[[j_star]]=c(reparto[[j_star]],tarea_elegida)
    restantes=setdiff(restantes,tarea_elegida)
  }
  matriz_costo_final=valoracionReparto(reparto,M)
  list(reparto=reparto,matriz_costo_final=matriz_costo_final)
}

# chauTareasFeas2BestAlfaEf es la versión de ChauTareasFeas2
# en la cual el criterio de selección del agente a la que se
# le asigna la tarea es mediante la busqueda del mejor leximin
# entre los postulantes.
chauTareasFeas2BestAlfaEf = function(valoraciones){
  n_tareas = dim(valoraciones)[1]
  n_agentes = dim(valoraciones)[2]
  M = proporciones(valoraciones)

  sorteo = function(candidatos){
    candidatos[sample.int(length(candidatos), 1)]
  }

  agente_mas_cargado = function(reparto){
    costos = diag(valoracionReparto(reparto, M))
    sorteo(which(costos == max(costos)))
  }

  tarea_mas_pesada = function(restantes, agente){
    valoracion_restantes = M[restantes, agente]
    restantes[sorteo(which(valoracion_restantes == max(valoracion_restantes)))]
  }

  reparto = vector(mode="list", length=n_agentes)
  restantes = 1:n_tareas
  while(length(restantes) > 0){
    i_star = agente_mas_cargado(reparto)
    tarea_elegida = tarea_mas_pesada(restantes, i_star)
    postulantes = (1:n_agentes)[-i_star]
    alfa_ef_vals = sapply(postulantes, function(j){
      reparto_tentativo = reparto
      reparto_tentativo[[j]] = c(reparto[[j]], tarea_elegida)
      envidia2_tareas(valoraciones, reparto_tentativo)$alfa_ef
    })
    mejor_alfa_ef = min(alfa_ef_vals)
    j_star = sorteo(postulantes[alfa_ef_vals == mejor_alfa_ef])
    reparto[[j_star]] = c(reparto[[j_star]], tarea_elegida)
    restantes = setdiff(restantes, tarea_elegida)
  }
  matriz_costo_final = valoracionReparto(reparto, M)
  list(reparto=reparto, matriz_costo_final=matriz_costo_final)
}

# Fijado en n_tareas y la cantidad de agentes, busco el c 
# tal que esp_maximos(c,n_tareas)=1/n_agentes.
# Con esta función deberíamos poder encontrar un 'c' que nos haga
# encontrar cotizaciones con dirichlet lo suficientemente complejas.
# Esta función se debería correr una sola vez para encontrar esos 'c'
# para cada tupla de cantAgentes, cantTareas
esp_maximos=function(c,n_tareas){
  maximos=vector(,length=1000)
    for(i in 1:1000){
    alfa=rep(c,n_tareas)
    maximos[i]=max(rdirichlet(1,alfa))  
  }
  mean(maximos)
}

# Tabla de los 'c' que cumplen esp_maximos(c,tareas)=1/agentes, para cada tupla
# (agentes, tareas) que usamos en las simulaciones. Calculada una sola vez con
# buscar_c() en localrun.R; es una estimación Monte Carlo, con error ~0.001.
valores_c = data.frame(
  agentes = c(  2,   2,   2,   2,   3,   3,   3,   3,   4,   4,   4,   4,
                2,   2,   2,   2,   3,   3,   3,   3,   4,   4,   4,   4),
  tareas  = c(  6,   9,  12,  15,   6,   7,   8,   9,   5,   6,   7,   8,
               20,  25,  30,  35,  15,  20,  25,  30,  10,  15,  20,  25),
  c       = c(0.519798, 0.277929, 0.189463, 0.144331,   # 2 agentes, exhaustivos
              2.047168, 1.404768, 1.055133, 0.848103,   # 3 agentes, exhaustivos
              23.142463, 7.591093, 4.093397, 2.705078,  # 4 agentes, exhaustivos
              0.103716, 0.079383, 0.065265, 0.054984,   # 2 agentes, no exhaustivos
              0.380984, 0.259959, 0.198221, 0.159328,   # 3 agentes, no exhaustivos
              1.584675, 0.744774, 0.482087, 0.356434)   # 4 agentes, no exhaustivos
)

comparar_algoritmos = function(n_tests, n_tareas=12, n_agentes=3, n_iter=1000, segundos=3){
  nombres = c("chau_tareas_feas", "repartoTareas", "chauTareasFeas2", "repartoTareasTopTrading",
              "repartoTareasTopTradingRandom", "repartoTareasTopTradingRandomLastEnvyCycle",
              "chauTareasFeas2Random", "chauTareasFeas2BestAlfaEf")
  victorias_alfa_ef = setNames(integer(8), nombres)
  victorias_leximin = setNames(integer(8), nombres)
  totales_alfa_ef   = matrix(nrow=n_tests, ncol=8, dimnames=list(NULL, nombres))
  totales_leximin   = matrix(nrow=n_tests, ncol=8, dimnames=list(NULL, nombres))

  reparto_inicial = function(valoraciones){
    reparto = vector(mode="list", length=dim(valoraciones)[2])
    reparto[[1]] = 1:dim(valoraciones)[1]
    reparto
  }

  mejores_de_n = function(algoritmo, valoraciones, n_iter){
    reparto_elegido_alfa    = reparto_inicial(valoraciones)
    reparto_elegido_leximin = reparto_inicial(valoraciones)
    mejor_alfa_ef           = Inf
    mejor_carga_leximin     = Inf

    for(i in 1:n_iter){
      reparto_aux  = algoritmo(valoraciones)
      envidia_func = envidia2_tareas(valoraciones, reparto_aux$reparto)

      if(envidia_func$alfa_ef < mejor_alfa_ef){
        mejor_alfa_ef        = envidia_func$alfa_ef
        reparto_elegido_alfa = reparto_aux$reparto
      }

      if(comparacion_leximin_pp_tareas(reparto_elegido_leximin, reparto_aux$reparto, valoraciones) == 2){
        reparto_elegido_leximin = reparto_aux$reparto
        mejor_carga_leximin     = reparto_aux$carga_total
      }
    }

    list(alfa_ef = mejor_alfa_ef, carga_leximin = mejor_carga_leximin)
  }

  mejores_n_segundos = function(algoritmo, valoraciones, segundos){
    reparto_elegido_alfa    = reparto_inicial(valoraciones)
    reparto_elegido_leximin = reparto_inicial(valoraciones)
    mejor_alfa_ef           = Inf
    mejor_carga_leximin     = Inf

    tiempo_inicio = Sys.time()
    while(as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs")) < segundos){
      reparto_aux  = algoritmo(valoraciones)
      envidia_func = envidia2_tareas(valoraciones, reparto_aux$reparto)

      if(envidia_func$alfa_ef < mejor_alfa_ef){
        mejor_alfa_ef        = envidia_func$alfa_ef
        reparto_elegido_alfa = reparto_aux$reparto
      }

      if(comparacion_leximin_pp_tareas(reparto_elegido_leximin, reparto_aux$reparto, valoraciones) == 2){
        reparto_elegido_leximin = reparto_aux$reparto
        mejor_carga_leximin     = reparto_aux$carga_total
      }
    }

    list(alfa_ef = mejor_alfa_ef, carga_leximin = mejor_carga_leximin)
  }

  wrapper_0 = function(valoraciones){
    res = chau_tareas_feas(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_1 = function(valoraciones){
    res = repartoTareas(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_2 = function(valoraciones){
    res = chauTareasFeas2(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_3 = function(valoraciones){
    res = repartoTareasTopTrading(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_4 = function(valoraciones){
    res = repartoTareasTopTradingRandom(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_5 = function(valoraciones){
    res = repartoTareasTopTradingRandomLastEnvyCycle(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_6 = function(valoraciones){
    res = chauTareasFeas2Random(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_7 = function(valoraciones){
    res = chauTareasFeas2BestAlfaEf(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }

  wrappers = list(wrapper_0, wrapper_1, wrapper_2, wrapper_3, wrapper_4, wrapper_5, wrapper_6, wrapper_7)

  for(t in 1:n_tests){
    cotizacion = rdirichlet(1, rep(1, n_tareas))
    valoraciones = t(rdirichlet(n_agentes, as.vector(cotizacion)*1000))

    for(a in 1:8){
      # ejecutamos el algoritmo N cantidad de veces
      # res = mejores_de_n(wrappers[[a]], valoraciones, n_iter)
      # ejecutamos el algoritmo la mayor cantidad de veces que podamos en N segundos
      res = mejores_n_segundos(wrappers[[a]], valoraciones, segundos)

      # almacenamos el ganador del alfa ef
      totales_alfa_ef[t, a] = res$alfa_ef
      # almacenamos el ganador de leximin
      totales_leximin[t, a] = res$carga_leximin
    }

    ganador_alfa_ef = which(totales_alfa_ef[t, ] == min(totales_alfa_ef[t, ]))
    victorias_alfa_ef[ganador_alfa_ef] = victorias_alfa_ef[ganador_alfa_ef] + 1

    ganador_leximin = which(totales_leximin[t, ] == min(totales_leximin[t, ]))
    victorias_leximin[ganador_leximin] = victorias_leximin[ganador_leximin] + 1

    cat(sprintf("Test %d/%d\talfa_ef: %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f — winner: %s\n",
                t, n_tests, totales_alfa_ef[t,1], totales_alfa_ef[t,2], totales_alfa_ef[t,3],
                totales_alfa_ef[t,4], totales_alfa_ef[t,5], totales_alfa_ef[t,6],
                totales_alfa_ef[t,7], totales_alfa_ef[t,8],
                paste(nombres[ganador_alfa_ef], collapse=", ")))
    cat(sprintf("\t\tleximin: %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f — winner: %s\n",
                totales_leximin[t,1], totales_leximin[t,2], totales_leximin[t,3],
                totales_leximin[t,4], totales_leximin[t,5], totales_leximin[t,6],
                totales_leximin[t,7], totales_leximin[t,8],
                paste(nombres[ganador_leximin], collapse=", ")))
  }

  cat("\n===== Results (alfa_ef) =====\n")
  for(a in 1:8){
    cat(sprintf("%-50s  wins: %d/%d (%.1f%%)\tavg alfa_ef: %.4f\n",
                nombres[a], victorias_alfa_ef[a], n_tests,
                100*victorias_alfa_ef[a]/n_tests, mean(totales_alfa_ef[,a])))
  }

  cat("\n===== Results (leximin) =====\n")
  for(a in 1:8){
    cat(sprintf("%-50s  wins: %d/%d (%.1f%%)\tavg carga leximin: %.4f\n",
                nombres[a], victorias_leximin[a], n_tests,
                100*victorias_leximin[a]/n_tests, mean(totales_leximin[,a])))
  }

  list(
    victorias_alfa_ef = victorias_alfa_ef,
    totales_alfa_ef   = totales_alfa_ef,
    victorias_leximin = victorias_leximin,
    totales_leximin   = totales_leximin
  )
}

# Parametros para simulación:
# LoteInicio: en las funciones de Agustin este seria nRep1i. marca el inicio del lote que vamos a ejecutar
# LoteFin: en las funciones de Agustin este seria nRep1f. marca el fin del lote que vamos a ejecutar
# CantidadDeInstanciasPorCotizacion: cuando formemos una cotizacion para un lote, vamos a crear esta cantidad de instancias que vamos a usar para correr todos los algoritmos
# CantidadDeSegundosPorAlgoritmo: vamos a ejecutar cada algoritmo aleatorio esta cierta cantidad de segundos con la misma instancia y nos quedamos con el mejor leximin/alfaEF, etc.
# Lambda: lambda a aplicar en la función dirichlet para formar las instancias a partir de la cotizacion. Por ej: 10, 100 o 1000
# Agentes: cantidad de agentes de la simulación. Por ej: 3
# Tareas: cantidad de tareas de la simulación. Por ej: 15
#
# Los tres últimos parámetros son escalares a propósito: una función de orden
# superior es la que va a permutar sobre lambdas y tuplas (agentes, tareas).
#
# Ejemplo de corridas:
# lambdas (10, 100, 1000)
#
# PRUEBA 1:
# El exhaustivo Agustin lo corrio para
# 2 agentes 6, 9, 12, 15 tareas
# 3 agentes 6, 7, 8, 9 tareas
# 4 agentes 5, 6, 7, 8 tareas
# Correr los no exhaustivos con este mismo universo de tarea/agentes y contrastar con el exhaustivo
#
#
# PRUEBA 2:
# no exhaustivos:
# 2 agentes 20, 25, 30, 35 tareas
# 3 agentes 15, 20, 25, 30 tareas
# 4 agentes 10, 15, 20, 25 tareas
#
# Sobre las semillas: set.seed fija las *instancias*, no los resultados. Los
# algoritmos son todos aleatorios y consumen del mismo stream del RNG después
# del set.seed, y como el presupuesto es por tiempo la cantidad de iteraciones
# cambia según la máquina y su carga. O sea: el archivo de instancias es
# reproducible, el de resultados no.
#
# Duración aproximada de una corrida:
#   (LoteFin - LoteInicio + 1) * CantidadDeInstanciasPorCotizacion * 8 * CantidadDeSegundosPorAlgoritmo
# segundos. Los 8 algoritmos son aleatorios, así que todos consumen el
# presupuesto completo. Por ej: 50 lotes * 5 instancias * 8 * 3s ~ 100 minutos.
#
# Dudas:
# ✅ En las corridas que me mencionó Agustín, el corre el exhaustivo con un set de agentes/tareas y el no exhaustivo con otro set distinto
# Como es que comparamos después? -> Respuesta: se corren con el mismo universo tanto el exhaustivo como el no exhaustivo
#
# ✅ Tengo dudas todavía de como generé la tabla de C's
#
# En las simulaciones de Agustín se evaluan muchos parametros. Yo tengo solo alfaEF y Leximin (que son los que me traje a mi función de simulación)
# Voy a necesitar una mano con adaptar los otros criterios de evaluación para tareas. (Verdad verdadera, Nash, Leximin)
# ✅ Respondido: tengo que adaptar. Ver abajo.
#
# Pasos a seguir:
# Tenemos que para cada reparto analizar los 6 atributos en los no exhaustivos
# AlfaEF
# AlfaEFX
# AlfaEF1
# AlfaPROP
# AlfaPROP1
# AlfaPROPX
# Leximin
# BienestarNash
# MasTrabaja (El análogo a menosLleva de bienes)
# DesutilidadSocial (suma de las desutilidades del reparto. Ver 'socials' en reparto sinExh)
# 
# Y para el exhaustivo sería
# Todo lo de arriba
# + mejor Lexi
simulacionTareas = function(LoteInicio, LoteFin,
                            CantidadDeInstanciasPorCotizacion,
                            CantidadDeSegundosPorAlgoritmo,
                            Lambda, Agentes, Tareas){

  n_agentes = Agentes
  n_tareas  = Tareas

  nombres = c("chau_tareas_feas", "repartoTareas", "chauTareasFeas2", "repartoTareasTopTrading",
              "repartoTareasTopTradingRandom", "repartoTareasTopTradingRandomLastEnvyCycle",
              "chauTareasFeas2Random", "chauTareasFeas2BestAlfaEf")
  abrev   = c("chau", "mio", "chau2", "tt", "tt_rnd", "tt_rnd_lec", "chau2_rnd", "chau2_alfaef")


  num_lotes = LoteFin - LoteInicio + 1
  num_filas = num_lotes * CantidadDeInstanciasPorCotizacion

  victorias_alfa_ef         = setNames(integer(8), nombres)
  victorias_leximin         = setNames(integer(8), nombres)
  totales_alfa_ef           = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_alfa_ef1          = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_alfa_efx          = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_alfa_prop         = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_alfa_prop1        = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_alfa_propx        = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_leximin           = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_bienestar_nash    = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  totales_iters      = matrix(nrow=num_filas, ncol=8, dimnames=list(NULL, nombres))
  lote_col          = vector(, length=num_filas)
  caso_col          = vector(, length=num_filas)

  reparto_inicial = function(valoraciones){
    reparto = vector(mode="list", length=dim(valoraciones)[2])
    reparto[[1]] = 1:dim(valoraciones)[1]
    reparto
  }

  # Corremos el algoritmo recibido por una cantidad de segundos
  # y nos quedamos con el reparto con el mejor Leximin.
  mejores_n_segundos = function(algoritmo, valoraciones, segundos){
    reparto_elegido_alfa    = reparto_inicial(valoraciones)
    reparto_elegido_leximin = reparto_inicial(valoraciones)
    iteraciones             = 0

    tiempo_inicio = Sys.time()
    while(as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs")) < segundos){
      iteraciones  = iteraciones + 1
      reparto_aux  = algoritmo(valoraciones)

      if(iteraciones == 1 ||
         comparacion_leximin_pp_tareas(reparto_elegido_leximin, reparto_aux$reparto, valoraciones) == 2){
        reparto_elegido_leximin = reparto_aux$reparto
      }
    }

    # La carga la sacamos del reparto ganador en vez de arrastrarla por el loop:
    # así no dependemos de que cada wrapper setee carga_total.
    carga_leximin = max(diag(valoracionReparto(reparto_elegido_leximin, valoraciones)))
    envidia_func = envidia2_tareas(valoraciones, reparto_elegido_leximin)

    list(
      alfa_ef       = envidia_func$alfa_ef,
      alfa_ef1      = envidia_func$alfa_ef1,
      alfa_efx      = envidia_func$alfa_efx,
      alfa_prop     = envidia_func$alfa_prop,
      alfa_prop1    = envidia_func$alfa_prop1,
      alfa_propx    = envidia_func$alfa_propx,
      bienestar_nash = envidia_func$bienestar_nash,
      carga_leximin = carga_leximin,
      iteraciones   = iteraciones
    )
  }

  wrapper_0 = function(valoraciones){
    res = chau_tareas_feas(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_1 = function(valoraciones){
    res = repartoTareas(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_2 = function(valoraciones){
    res = chauTareasFeas2(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_3 = function(valoraciones){
    res = repartoTareasTopTrading(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_4 = function(valoraciones){
    res = repartoTareasTopTradingRandom(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_5 = function(valoraciones){
    res = repartoTareasTopTradingRandomLastEnvyCycle(n_agentes, valoraciones)
    res$reparto = res$Art
    res$carga_total = max(res$llevan)
    res
  }
  wrapper_6 = function(valoraciones){
    res = chauTareasFeas2Random(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }
  wrapper_7 = function(valoraciones){
    res = chauTareasFeas2BestAlfaEf(valoraciones)
    res$reparto = res$reparto
    res$carga_total = max(diag(res$matriz_costo_final))
    res
  }

  wrappers = list(wrapper_0, wrapper_1, wrapper_2, wrapper_3, wrapper_4, wrapper_5, wrapper_6, wrapper_7)

  # Corre los 8 algoritmos sobre una misma instancia. Es la unidad de trabajo que
  # le mandamos a cada worker: los 8 quedan seriales dentro del mismo proceso.
  correr_instancia = function(valoraciones){
    lapply(wrappers, function(w) mejores_n_segundos(w, valoraciones, CantidadDeSegundosPorAlgoritmo))
  }

  # --- Archivos de salida ---
  sufijo = paste("_", n_tareas, "_tareas_", n_agentes, "_agen_",
                 "nrep1i_", LoteInicio, "_nrep1f_", LoteFin,
                 "_ninst_", CantidadDeInstanciasPorCotizacion,
                 "_lambda_", Lambda, ".txt", sep="")
  archivo_resultados = paste("reparto_TAREAS", sufijo, sep="")
  archivo_instancias = paste("instancia_TAREAS", sufijo, sep="")

  columnas  = c("rep", "caso",
                paste("alfa_ef_",    abrev, sep=""),
                paste("alfa_ef1_",   abrev, sep=""),
                paste("alfa_efx_",   abrev, sep=""),
                paste("alfa_prop_",  abrev, sep=""),
                paste("alfa_prop1_", abrev, sep=""),
                paste("alfa_propx_", abrev, sep=""),
                paste("bienNash_",   abrev, sep=""),
                paste("leximin_",    abrev, sep=""),
                paste("iters_",      abrev, sep=""))
  columnas2 = c("rep", "caso", rep(1:n_tareas, n_agentes))
  write.table(t(columnas),  file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(t(columnas2), file = archivo_instancias, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

  # --- Simulación ---
  # El alfa de la Dirichlet que genera la cotización no es 1: usamos el c
  # calibrado para esta tupla (agentes, tareas) de la tabla valores_c.
  caso_c = which(valores_c$agentes == n_agentes & valores_c$tareas == n_tareas)
  if(length(caso_c) == 0){
    stop(sprintf("valores_c no tiene un c para %d agentes y %d tareas", n_agentes, n_tareas))
  }
  valor_c = valores_c$c[caso_c]
  alfaVec = rep(valor_c, n_tareas)

  # Cada fila (lote, caso) es independiente, así que las repartimos entre workers.
  # El paralelismo va a nivel de fila y no de los 8 algoritmos a propósito: como el
  # presupuesto de cada algoritmo es de reloj, correr los 8 en procesos distintos los
  # deja a merced de en qué núcleo los ponga el sistema (los de eficiencia son
  # bastante más lentos), y eso sesgaría justo la comparación que queremos medir.
  # Con una fila por worker los 8 comparten proceso y núcleo: si a una fila le toca un
  # núcleo lento, itera menos en los 8 por igual y la comparación interna no cambia.
  n_workers = max(1L, detectCores() - 1L)

  # Generamos todas las instancias acá, en el mismo orden y con las mismas semillas
  # de siempre: así el archivo de instancias no depende del paralelismo.
  instancias = vector("list", num_filas)
  fila = 0
  for(i in LoteInicio:LoteFin){
    set.seed(500+i)
    X1 = rdirichlet(1, alfaVec)   # cotización del lote

    for(j in 1:CantidadDeInstanciasPorCotizacion){
      set.seed(1000+CantidadDeInstanciasPorCotizacion*i+j)
      fila = fila + 1
      lote_col[fila] = i
      caso_col[fila] = j
      instancias[[fila]] = t(rdirichlet(n_agentes, Lambda*(as.vector(X1))))   # instancia
    }
  }

  # De acá en adelante el RNG lo consumen los algoritmos, que corren dentro de los
  # workers. Un fork le copia al hijo el .Random.seed del padre, así que sin esto los
  # workers sortearían todos lo mismo; L'Ecuyer-CMRG le da a cada uno un stream
  # disjunto. Va después de generar las instancias, y se restaura al salir, porque
  # set.seed produce números distintos según el RNGkind activo.
  rng_previo = RNGkind()
  RNGkind("L'Ecuyer-CMRG")
  on.exit(RNGkind(rng_previo[1], rng_previo[2], rng_previo[3]), add = TRUE)

  # Vamos por chunks de n_workers filas, en vez de mandar todo junto, para no perder
  # la escritura incremental a archivo ni el progreso por pantalla.
  chunks = split(seq_len(num_filas), ceiling(seq_len(num_filas)/n_workers))

  for(chunk in chunks){
    # cada worker corre los 8 algoritmos de una fila la mayor cantidad de veces
    # que pueda en N segundos
    res_chunk = mclapply(instancias[chunk], correr_instancia, mc.cores = n_workers)

    # mclapply no aborta: deja un try-error (o NULL) en la posición que falló.
    if(any(!vapply(res_chunk, is.list, logical(1)))){
      stop("falló un worker de mclapply; ver el mensaje de error de arriba")
    }

    for(k in seq_along(chunk)){
      fila         = chunk[k]
      res_fila     = res_chunk[[k]]
      valoraciones = instancias[[fila]]
      i            = lote_col[fila]
      j            = caso_col[fila]

      for(a in 1:8){
        res = res_fila[[a]]

        totales_alfa_ef[fila, a]    = res$alfa_ef
        totales_alfa_ef1[fila, a]   = res$alfa_ef1
        totales_alfa_efx[fila, a]   = res$alfa_efx
        totales_alfa_prop[fila, a]  = res$alfa_prop
        totales_alfa_prop1[fila, a] = res$alfa_prop1
        totales_alfa_propx[fila, a] = res$alfa_propx
        totales_bienestar_nash[fila, a]    = res$bienestar_nash
        totales_leximin[fila, a]    = res$carga_leximin
        totales_iters[fila, a]      = res$iteraciones
      }

      ganador_alfa_ef = which(totales_alfa_ef[fila, ] == min(totales_alfa_ef[fila, ]))
      victorias_alfa_ef[ganador_alfa_ef] = victorias_alfa_ef[ganador_alfa_ef] + 1

      ganador_leximin = which(totales_leximin[fila, ] == min(totales_leximin[fila, ]))
      victorias_leximin[ganador_leximin] = victorias_leximin[ganador_leximin] + 1

      # --- Escritura en archivo ---
      guardo      = c(i, j,
                      totales_alfa_ef[fila, ], totales_alfa_ef1[fila, ], totales_alfa_efx[fila, ],
                      totales_alfa_prop[fila, ], totales_alfa_prop1[fila, ], totales_alfa_propx[fila, ],
                      totales_bienestar_nash[fila, ], totales_leximin[fila, ], totales_iters[fila, ])
      guardo_inst = c(i, j, as.vector(valoraciones))

      write.table(t(guardo),      file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(t(guardo_inst), file = archivo_instancias, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)

      cat(sprintf("%d_agentes_%d_tareas_lambda_%s_lote_%d_caso_%d (%d/%d)\n",
                  n_agentes, n_tareas, format(Lambda), i, j, fila, num_filas))
      cat(sprintf("\talfa_ef: %s — ganador: %s\n",
                  paste(sprintf("%.4f", totales_alfa_ef[fila, ]), collapse=" | "),
                  paste(nombres[ganador_alfa_ef], collapse=", ")))
      cat(sprintf("\talfa_ef1: %s\n",
                  paste(sprintf("%.4f", totales_alfa_ef1[fila, ]), collapse=" | ")))
      cat(sprintf("\talfa_efx: %s\n",
                  paste(sprintf("%.4f", totales_alfa_efx[fila, ]), collapse=" | ")))
      cat(sprintf("\talfa_prop: %s\n",
                  paste(sprintf("%.4f", totales_alfa_prop[fila, ]), collapse=" | ")))
      cat(sprintf("\talfa_prop1: %s\n",
                  paste(sprintf("%.4f", totales_alfa_prop1[fila, ]), collapse=" | ")))
      cat(sprintf("\talfa_propx: %s\n",
                  paste(sprintf("%.4f", totales_alfa_propx[fila, ]), collapse=" | ")))
      cat(sprintf("\tbienNash: %s\n",
                  paste(sprintf("%.4f", totales_bienestar_nash[fila, ]), collapse=" | ")))
      cat(sprintf("\tleximin: %s — ganador: %s\n",
                  paste(sprintf("%.4f", totales_leximin[fila, ]), collapse=" | "),
                  paste(nombres[ganador_leximin], collapse=", ")))
    }
  }

  cat("\n===== Results (alfa_ef) =====\n")
  for(a in 1:8){
    cat(sprintf("%-50s  wins: %d/%d (%.1f%%)\tavg alfa_ef: %.4f\n",
                nombres[a], victorias_alfa_ef[a], num_filas,
                100*victorias_alfa_ef[a]/num_filas, mean(totales_alfa_ef[,a])))
  }

  cat("\n===== Results (leximin) =====\n")
  for(a in 1:8){
    cat(sprintf("%-50s  wins: %d/%d (%.1f%%)\tavg carga leximin: %.4f\n",
                nombres[a], victorias_leximin[a], num_filas,
                100*victorias_leximin[a]/num_filas, mean(totales_leximin[,a])))
  }

  cat(sprintf("\nResultados: %s\nInstancias: %s\n", archivo_resultados, archivo_instancias))

  resultados = data.frame(rep = lote_col, caso = caso_col)
  resultados[paste("alfa_ef_",    abrev, sep="")] = totales_alfa_ef
  resultados[paste("alfa_ef1_",   abrev, sep="")] = totales_alfa_ef1
  resultados[paste("alfa_efx_",   abrev, sep="")] = totales_alfa_efx
  resultados[paste("alfa_prop_",  abrev, sep="")] = totales_alfa_prop
  resultados[paste("alfa_prop1_", abrev, sep="")] = totales_alfa_prop1
  resultados[paste("alfa_propx_", abrev, sep="")] = totales_alfa_propx
  resultados[paste("bienNash_",   abrev, sep="")] = totales_bienestar_nash
  resultados[paste("leximin_",    abrev, sep="")] = totales_leximin
  resultados[paste("iters_",      abrev, sep="")] = totales_iters

  list(
    resultados         = resultados,
    victorias_alfa_ef  = victorias_alfa_ef,
    victorias_leximin  = victorias_leximin,
    archivo_resultados = archivo_resultados,
    archivo_instancias = archivo_instancias
  )
}
