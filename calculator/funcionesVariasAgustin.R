#install.packages("sandwich")
#install.packages("igraph")
library("igraph")
#install.packages("isoband")
library("isoband")
#install.packages("sandwich")
library("sandwich")
#install.packages("DirichletReg")
library("DirichletReg")
#install.packages("shiny")
#library("shiny")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("plotly")
#library("plotly")
#install.packages("gridExtra")
library("gridExtra")
#install.packages("partitions")
library("partitions")
####################
# asignacion
####################
# entrada:
# un vector x de n coordenadas (por los n bienes). Cada coordenada un número enrte 1 y k (a quien corresponde el bien)
# salida: 
# una lista de k vectores. lista[[i]] son los bienes que se lleva el agente i
asignacion=function(x,n,k){
  lista=list()
  for(i in 1:k){
    lleva=which(x==i)
    lista[[i]]=lleva
  }
  return(lista)
}

# asignacionInv
####################
# entrada:
# una lista de k vectores. lista[[i]] son los bienes que se lleva el agente i
# salida: 
# un vector x de n coordenadas (por los n bienes). Cada coordenada un número enrte 1 y k (a quien corresponde el bien)
####################
asignacionInv = function(reparto){
  nAgentes=length(reparto)
  nBienes=0
  for(i in 1:nAgentes){
    nBienes=nBienes+length(reparto[[i]])
  }
  vectorRta=vector(,length = nBienes)
  for(i in 1:nAgentes){
    vectorRta[reparto[[i]]]=i
  }
  return(vectorRta)
}

#############
# reemplazo es una función
# que dado a un vector ordenado de mayor a menor, nos dice, si queremos sumar
# alguito menos que la k-ésima coordenada del vector, que 
# coordenadas del vector deberíamos usar.
#############
# entradas: 
# x -> el vector de n coordenadas
# k -> la coordenada que deseo reemplazar
reemplazo=function(x,k){
  nx=length(x)
  xDec=rev(sort(x))
  valor=xDec[k]
  if(k == nx-1){
    valores=k+1
    acum=xDec[k+1]
  }
  if(k<=nx-2){
    valores=k+1
    acum=xDec[k+1]
    for(i in (k+2):nx){
      if(acum+xDec[i]<valor){
        valores=c(valores,i)
        acum=acum+xDec[i]
      }
    }
  }
  list(acumulado=acum,valores=valores)
}



################
# dado un vector x y un valor v, busco qué coordenadas del vector x
# sumar para quedar cerca de v, por debajo.
################
sumaCerca= function(x,v){
  nx=length(x)
  if(nx==0){
    acum=0
    dife=v
    posiciones=c()
  }else{
    rangosVerre=nx+1-rank(x)  #el 1 es el más grande...
    xDec=rev(sort(x)) # el vector x ordenado decreciente
    acum=0
    posiciones=c()
    for(i in 1:nx){
      if(acum+xDec[i]<v){
        posicionNueva=which(rangosVerre==i)
        posiciones=c(posiciones,posicionNueva)
        acum=acum+xDec[i]
      }
    }
    dife=v-acum    
  }
  list(acum=acum,dife=dife,posiciones=posiciones)
}



#########
# Estaría bueno crear una función que a partir de las evaluaciones de cada agente
# y un reparto nos dice como están rankeados por el agente i lo que se lleva el agente j
# Luego la idea sería que el que más envidia siente le intente bajar el ranking o los rankings al 
# que más envidia le tiene a partir de que este intercambie con quien corresponda.
# Por ejemplo si el más envidiado se lleva el ranking 2 y 10 de quien más envidia. Entonces
# podríamos probar de que reciba 2 y 11 el más envidiado, o que reciba 3 y 9 o algo así.
# Y ver si esos pasos van disminuyendo la envidia máxima y repetir estos pasos.
##########


##################
# función rankings
# entradas:
# valoraciones -> matriz de nObj x kAgentes donde la columna j es la valuacion del j-esimo agente
# reparto -> una lista de k vectores, donde reparto[[i]] son los objetos que recibe el agente i
# salidas:
# una lista (rankeos) de kAgentes listas de kAgentes vectores
# donde rankeos[[i]][[j]] indica los rankings de lo que se lleva j según i
##################
rankings=function(valoraciones,reparto){
  nObjetos=dim(valoraciones)[1]
  kAgentes=dim(valoraciones)[2]
  props=proporciones(valoraciones)
  rankeos=nObjetos+1-apply(props,2,rank)
  a=list()
  for(i in 1:kAgentes){
    a[[i]]=list()
    for(j in 1:kAgentes){
      a[[i]][[j]]=rankeos[reparto[[j]],i]
    }
  }
  return(a)
}

##############
# función envidia
##############

envidia=function(valoraciones,reparto){
  S=valoracionReparto(reparto,valoraciones)
  props=proporciones(valoraciones)
  k1=dim(S)[1]
  envidiaMat=envyRatioMat=matrix(,k1,k1)
  envidian=vector(,length=k1)
  for(i in 1:k1){
    envidiaMat[i,]=S[i,]-S[i,i]
    envidian[i]=sum(envidiaMat[i,]>0)
    envyRatioMat[i,]=S[i,]/S[i,i]
    if(S[i,i]==0){envyRatioMat[i,i]=1}
  }
  maximaEnvidia=max(envidiaMat[row(envidiaMat)!=col(envidiaMat)])
  maximoEnvyRatio=max(envyRatioMat[row(envyRatioMat)!=col(envyRatioMat)])
  donde=which(envidiaMat == maximaEnvidia, arr.ind = TRUE)
  masEnvidioso=donde[1]
  masEnvidiado=donde[2]
  list(envidiaMat=envidiaMat,maximoEnvyRatio=maximoEnvyRatio,maximaEnvidia=maximaEnvidia,masEnvidioso=masEnvidioso,masEnvidiado=masEnvidiado,envidian=envidian)
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
      if(length(reparto[[j]])==0){
        denom1_ij=0}else{
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

proporcionalidad = function(repartido, valoraciones){
  n_agentes=dim(valoraciones)[2]
  n_bienes=dim(valoraciones)[1]
  prop_cond=prop1_cond=propX_cond=0
  props=proporciones(valoraciones)
  M=valoracionReparto(repartido,valoraciones)
  alfa_prop_vector=diag(M)*n_agentes
  alfa_prop_min=min(alfa_prop_vector)
  if(alfa_prop_min>=1){prop_cond=1}  # veo si el reparto es proporcional
  #ahora analizo prop1
  alfa_prop1_vector=vector(,length=n_agentes)
  for(i in 1:n_agentes){
    if(M[i,i]<1){#cuando i no siente que se lleva todo
      if(length(repartido[[i]])==0){
        maximo_agregable=max(props[,i])
      }else{
        maximo_agregable=max(props[-repartido[[i]],i])  
      }
      alfa_prop1_vector[i] = (M[i,i]+maximo_agregable)*n_agentes
    }else{
      alfa_prop1_vector[i]=1*n_agentes
    }
  }
  alfa_prop1_min=min(alfa_prop1_vector)
  if(alfa_prop1_min>=1){prop1_cond=1}
  
  #ahora analizo propX
  alfa_propX_vector=vector(,length=n_agentes)
  for(i in 1:n_agentes){
    if(M[i,i]<1){#cuando i no siente que se lleva todo
      if(length(repartido[[i]])==0){
        minimo_agregable=min(props[,i])
      }else{
        minimo_agregable=min(props[-repartido[[i]],i])  
      }
      alfa_propX_vector[i] = (M[i,i]+minimo_agregable)*n_agentes
    }else{
      alfa_propX_vector[i]=1*n_agentes
    }
  }
  alfa_propX_min=min(alfa_propX_vector)
  if(alfa_propX_min>=1){propX_cond=1}
  list(alfa_prop_vector=alfa_prop_vector,
       alfa_prop_min=alfa_prop_min,
       alfa_prop1_min=alfa_prop1_min,
       alfa_propX_min=alfa_propX_min,
       prop_cond=prop_cond,
       prop1_cond=prop1_cond,
       propX_cond=propX_cond)
}





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



#########
# productoria
##########
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


###################
# BienestarNash
###################

bienestarNash = function(matriz){
  a=productoria(diag(matriz))
  bienestarNashGral=a$producto
  bienestarNashSinCeros=a$productoSinCeros
  list(bienestarNashGral=bienestarNashGral,bienestarNashSinCeros=bienestarNashSinCeros)
}


###################
# BienestarSocial
###################

bienestarSocial = function(matriz){
  bienestar=sum(diag(matriz))
  bienestar
}

#####################
# EF1
#####################
# la función EF1 tiene 
# entradas:
# reparto: lista con los artículos que se lleva cada agente
# valoraciones:  matriz con las valoraciones por columna de cada agente
#
# salidas:
# ef1: que toma el valor 1 si el reparto es EF1 y 0 si no
# alfa: el máximo valor alfa para el cual el reparto es alfa-EF1

EF1 = function(reparto, valoraciones){
  props=proporciones(valoraciones)
  llevan=valoracionReparto(reparto,valoraciones)
  n=dim(valoraciones)[2]
  todos=1:n
  alfa=matrix(1,n,n)
  ef1=1
  for(i in todos){
    for(j in todos[-i]){
      max_ij=max(props[reparto[[j]],i]) # la proporción que representa para i el mejor artículo que se lleva j
      a=llevan[i,j]-max_ij #cuanto se lleva j según i si no le dieran a j el mejor artículo.
      if(a==0){
        alfa[i,j]=1
      }else{
        b=llevan[i,i]/a
        if(b<1){
          ef1=0
          alfa[i,j]=b
        }else{
          alfa[i,j]=1
        }
      }
    }
  }
  alfaMin=min(alfa)
  list(ef1=ef1,alfaMat=alfa,alfaMin=alfaMin)
} 


EFX = function(reparto, valoraciones){
  props=proporciones(valoraciones)
  llevan=valoracionReparto(reparto,valoraciones)
  n=dim(valoraciones)[2]
  todos=1:n
  alfa=matrix(1,n,n)
  efx=1
  for(i in todos){
    for(j in todos[-i]){
      if(length(reparto[[j]])>0){
        min_ij=min(props[reparto[[j]],i]) # la proporción que representa para i el peor artículo que se lleva j
        a=llevan[i,j]-min_ij #cuanto se lleva j según i si no le dieran a j el peor artículo.
        if(a==0){
          alfa[i,j]=1
        }else{
          b=llevan[i,i]/a
          if(b<1){
            efx=0
            alfa[i,j]=b
          }else{
            alfa[i,j]=1
          }
        }        
      }
    }
  }
  alfaMin=min(alfa)
  list(efx=efx,alfaMat=alfa,alfaMin=alfaMin)
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






# La función noEnvy tiene:
##########
# entrada:
##########
# matriz es la matriz de kxk de valoración, donde
# donde matriz[i,j] es lo que siente el heredero i que se lleva j

felicidad=function(matriz){
  S=matriz
  k1=dim(S)[1]
  k2=dim(S)[2]
  if(k1!=k2){stop("la matriz no es cuadrada")}
  envidiosos=0
  envidiadosLista=list()
  envidiadosCant=rep(0,k1)
  envidiaMaxima=rep(0,k1)
  menosLleva=min(diag(S))
  proporcionalidad=menosLleva>=1/k1
  for(i in 1:k1){
    a=which(S[i,]>S[i,i])
    if(length(a)>0){
      envidiadosLista[[i]]=a # a quienes envidia i
      envidiadosCant[i]=length(a)
      envidiosos=envidiosos+1
      envidiaMaxima[i]=max(S[i,a]-S[i,i])
    }
  }
  list(menosLleva=menosLleva,proporcionalidad=proporcionalidad,envidiosos=envidiosos,envidiadosLista=envidiadosLista,envidiadosCant=envidiadosCant,envidiaMaxima=envidiaMaxima)
}


###################
# función felicidad dependiendo del reparto y de las valoraciones
# la idea sería agregar a ver si cumple EF1 y EFX y ver cual EF1-alfa y EFX-alfa
# cumple el reparto
####################

felicidad2=function(repartido, valoraciones){
  S=valoracionReparto(repartido,valoraciones)
  k1=dim(S)[1]
  k2=dim(S)[2]
  if(k1!=k2){stop("la matriz no es cuadrada")}
  envidiosos=0
  envidiadosLista=list()
  envidiadosCant=rep(0,k1)
  envidiaMaxima=rep(0,k1)
  menosLleva=min(diag(S))
  bienSocial=sum(diag(S))
  bienNash=prod(diag(S))
  proporcionalidad=menosLleva>=1/k1
  for(i in 1:k1){
    a=which(S[i,]>S[i,i])
    if(length(a)>0){
      envidiadosLista[[i]]=a # a quienes envidia i
      envidiadosCant[i]=length(a)
      envidiosos=envidiosos+1
      envidiaMaxima[i]=max(S[i,a]-S[i,i])
    }
  }
  ef1=EF1(repartido,valoraciones)
  efx=EFX(repartido,valoraciones)
  ef=ifelse(sum(envidiaMaxima)==0,1,0)
  envidia_func=envidia2(valoraciones,repartido)
  alfa_ef=envidia_func$alfa_ef
  alfa_ef1=envidia_func$alfa_ef1
  alfa_efx=envidia_func$alfa_efx
  alfa_prop=envidia_func$alfa_prop
  alfa_prop1=envidia_func$alfa_prop1
  alfa_propx=envidia_func$alfa_propx
  list(alfa_ef=alfa_ef,
       alfa_ef1=alfa_ef1,
       alfa_efx=alfa_efx,
       alfa_prop=alfa_prop,
       alfa_prop1=alfa_prop1,
       alfa_propx=alfa_propx,
       ef=ef,
       ef1=ef1,
       efx=efx,
       menosLleva=menosLleva,
       proporcionalidad=proporcionalidad,
       envidiosos=envidiosos,
       envidiadosLista=envidiadosLista,
       envidiadosCant=envidiadosCant,
       envidiaMaxima=envidiaMaxima,
       bienSocial=bienSocial,
       bienNash=bienNash)
}

##############################
# Función reparto 
##############################
#la funcion reparto toma par?metros
#input:
#k: la cantidad de personas entre las que se reparte.
#M: una matriz de k columnas, donde cada columna es la valuaci?n del i?simo heredero
#que: toma los valores "tareas" y "bienes" dependiendo de qué quiero repartir

reparto=function(k,M){
  
    n=dim(M)[1]
    if(k != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
    Props=matrix(,n,k) # paso valores a proporciones 
    for(i in 1:k){
      Props[,i]=M[,i]/sum(M[,i])    
    }
    
    dif=array(,c(k,k,n)) #las diferencias e/ proporciones
    for(i in 1:k){
      for(j in 1:k){
        dif[i,j,]=Props[,i]-Props[,j]    
      }
    }
    
    difAbs=abs(dif)
    #crearemos una lista para saber los art?culos que le asignamos a cada heredero
    #La primera fila de la lista ser?n los art?culos que se lleva el primer heredero
    #y as? siguiendo.
    
    art=list()
    art[[k+1]]=1
    art[[k+1]]=c()
    for(i in 1:n){
      j=sample(which(Props[i,]==max(Props[i,])),1)
      art[[j]]=c(i,art[[j]])
    }
    
    #sumamos a ver cuanto siente que se lleva cada uno
    Lleva=vector(,length=k)
    for(i in 1:k){
      Lleva[i]=sum(Props[art[[i]],i])  
    }
    
    LlevaOrig=Lleva
    mini=min(Lleva)  #el m?nimo entre las proporciones que sienten que se llevan los hered.
    #este es el valor que se quiere maximizar.
    mini2=0
    # en caso que se lleve menos, intento darle a "a" 
    # el que tenga menor diferencia en favor de b
    # siempre respetando que no baje el m?nimo de los que se llevan ambos
    pasos=0 #para contar en cuantos pasos termina el algoritmo
    repeat{
      pasos=pasos+1
      
      for(j in 1:k){
        i=1
        if(mini==Lleva[j]){ # en caso que j se lleve menos, intento darle a "j"
          repeat{
            aa=which(difAbs[j,-j,]==sort(difAbs[j,-j,])[i]) #los lugares num?ricos de la matriz de la i?sima diferencia
            i=i+1
            naa=length(aa)
            articulo=vector(,naa) #los articulos con menor diferencia de ponderación
            pierde=vector(,naa) # los herederos que perderían los artículos
            for(jj in 1:naa){
              articulo[jj]=(aa[jj]-1)%/%(k-1)+1 #el art?culo que le vamos a dar a j si a?n no lo tiene
              if(aa[jj]%%(k-1)==0){
                pierde[jj]=((1:k)[-j])[k-1] #el que pierde el objeto
              }
              if(aa[jj]%%(k-1)!=0){
                pierde[jj]=((1:k)[-j])[aa[jj]%%(k-1)]
              }
              if(((articulo[jj] %in% art[[j]])==FALSE)&&(articulo[jj] %in% art[[pierde[jj]]])){
                
                # el heredero que pierde el art?culo
                
                art2=art
                art2[[j]]=c(art[[j]],articulo[jj])
                art2[[pierde[jj]]]=art[[pierde[jj]]][-(which(art[[pierde[jj]]]==articulo[jj]))]
                Lleva2=Lleva
                Lleva2[j]=Lleva[j]+Props[articulo[jj],j]
                Lleva2[pierde[jj]]=Lleva[pierde[jj]]-Props[articulo[jj],pierde[jj]]
                mini2=min(Lleva2)
              }
              if((mini2>mini)||(i>(k-1)*n)){
                break
              }  # me salgo del segundo repeat si logré subir lo que se lleva el que siente que menos se lleva o si no lo sirvió ningún artículo
            }
            if((mini2>mini)||(i>(k-1)*n)){
              break
            }
            
          }
        }
      }  
      
      #Hasta ac? intent? subir el m?nimo. si no subi? el m?nimo termina el algoritmo:
      if(mini>=mini2){
        break
      }
      #y si el m?nimo subi? actualizo los datos:
      art=art2
      Lleva=Lleva2
      mini=mini2
    }
    #try(if(mini<1/k) stop("Un participante se lleva menos de lo que deber?a. Se recomienda repetir quitando un art?culo caro y muy ponderado por ambos")) 
    return(list(Art=art,llevan=Lleva,llevanOrig=LlevaOrig,Pasos=pasos))    
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

########################
# Función repartoExhaustivo
#######################
# Entradas:
# n -> la cantidad de bienes que se reparten
# k -> la cantidad de agentes que reciben
# valoraciones -> la matriz de nxk donde cada columna son los valores de los n bienes para el heredero k
#
# Salidas:
# Los repartos que llevan a maximizar/ minimizar funciones objetivo
# bienNashSinCeros -> reparto que maximiza el bienestar de Nash cuando todos reciben
# bienNashCeros -> reparto que maximiza el bienestar de Nash cuando alguien no recibe nada
# bienSocial -> reparto que maximiza el bienestar social
# alfa_efx -> reparto que maximiza el mínimo de los alfa para alfa-EFX
# alfa_ef1 -> reparto que maximiza el mínimo de los alfa para alfa-EF1
# prop -> reparto que maximiza el mínimo de las proporciones que sienten que se llevan
#
# bienNashSinCerosVal -> valor máximo del bienestar de Nash cuando todos reciben
# bienNashCerosVal -> valor máximo del bienestar de Nash cuando alguien no recibe nada
# bienSocialVal -> valor máximo del bienestar social
# alfa_efxVal -> valor máximo del mínimo de los alfa para alfa-EFX
# alfa_ef1Val -> valor máximo del mínimo de los alfa para alfa-EF1
# propVal -> valor máximo del mínimo de las proporciones que sienten que se llevan

# con n=13 y k = 3 demora entre 1 y 2 minutos probando 1594326 repartos
# con n=20 y k = 2 demora cerca de 1 minuto (razonable para simulaciones)


repartoExhaustivo=function(n,k,valoraciones){
  a=sum.comb(n,k)
  a=a[!apply(a,1,is.unsorted),]
  largo=dim(a)[1]
  menosLlevaMax=0
  miniMaxEnvidia=1
  formas=0
  reparto_leximin_opt=list()
  reparto_leximin_opt[[k]]=1:n
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
        valoracion=valoracionReparto(repartido,valoraciones) #matriz de valoracion del reparto
        menosLleva=min(diag(valoracion))
        bienSocial=sum(diag(valoracion))
        bienNash=(prod(diag(valoracion)))^(1/k)
        envidiaMaxima=rep(0,k)
        for(t in 1:k){
          envidiaMaxima[t]=max(valoracion[t,-t]-valoracion[t,t])
        }
        maximaEnvidia=max(envidiaMaxima)
        if(menosLleva>menosLlevaMax){
          menosLlevaMax=menosLleva
          menosLlevaRep=repartido
          bienSocialMaxiMin=bienSocial
          bienNashMaxiMin=bienNash
        }
        if(maximaEnvidia<miniMaxEnvidia){
          miniMaxEnvidia=maximaEnvidia
          miniMaxEnvidiaRep=repartido
          bienSocialMiniMaxEnvidia=bienSocial
          bienNashMiniMaxEnvidia=bienNash
        }
        if(comparacion_leximin_pp(repartido,reparto_leximin_opt,valoraciones)==1){
          reparto_leximin_opt=repartido
        }
      }
    }
  }
  list(formas=formas,menosLlevaMax=menosLlevaMax,menosLlevaRep=menosLlevaRep,bienSocialMaxiMin=bienSocialMaxiMin,miniMaxEnvidia=miniMaxEnvidia,miniMaxEnvidiaRep=miniMaxEnvidiaRep,bienSocialMiniMaxEnvidia=bienSocialMiniMaxEnvidia,bienNashMiniMaxEnvidia=bienNashMiniMaxEnvidia,bienNashMaxiMin=bienNashMaxiMin,reparto_leximin_opt=reparto_leximin_opt)
}









#####################
# Reparto exhaustivo
####################    

repartoExhaustivoEFX2=function(n,k,valoraciones){
  a=sum.comb(n,k)
  a=a[!apply(a,1,is.unsorted),]
  largo=dim(a)[1]
  alfaResult=0
  envidiaMaxima=1
  nashResult=0 #nuevo
  formas=0
  proporcionalidad=0
  maximin=0
  reparto_prop=vector(mode="list",length=k)
  reparto_prop[[1]]=1:n
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
        valoracion=valoracionReparto(repartido,valoraciones) #matriz de valoracion del reparto
        alfaAux=EFX(repartido,valoraciones)$alfaMin
        envidiaMaximaAux=envidia(valoraciones,repartido)$maximaEnvidia
        menos_recibe=min(diag(valoracion))
        if(menos_recibe>=1/k){ #si llega a ser proporcional
          proporcionalidad=1
          if(comparacion_leximin_pp(reparto_prop,repartido,valoraciones)==2){
            reparto_prop=repartido
          }
        }
        if(menos_recibe>maximin){maximin=menos_recibe}
        
        nashAux=bienestarNash(valoracion)$bienestarNashGral #nuevo
        if((alfaAux==1)&(envidiaMaximaAux<envidiaMaxima)){
          repartidoResult=repartido
          alfaResult=alfaAux
          envidiaMaxima=envidiaMaximaAux
          #return(list(alfa=alfaResult,repartido=repartidoResult))
        }
        if(nashAux>nashResult){#nuevo
          repartidoNash=repartido
          nashResult=nashAux
        }
        if(alfaAux>alfaResult){
          repartidoResult=repartido
          alfaResult=alfaAux
          envidiaMaxima=envidiaMaximaAux
        }
      }
    }
  }
  return(list(alfa=alfaResult,repartido=repartidoResult,envidiaMaxima=envidiaMaxima,repartidoNash=repartidoNash,nashMax=nashResult,reparto_prop=reparto_prop,proporcionalidad=proporcionalidad,maximin=maximin))
}


repartoExhaustivo_v2=function(valoraciones){
  k=dim(valoraciones)[2] #la cantidad de agentes
  n=dim(valoraciones)[1] #la cantidad de bienes.
  a=compositions(n,k)
  a=a[,!apply(a,2,is.unsorted)]
  a=t(a)
  largo=dim(a)[1]
  #alfa_X_Result=0
  envy_ratio_max=Inf
  #envy_ratio_max_X=Inf  #me va a interesar al buscar reparto EFX que vaya disminuyendo el envy_ratio_max
  nashResult=0 #nuevo
  nash_posit=0
  menosLleva = 0 #(esto habría que maximizarlo entiendo)       ---------------------------------- NUEVO
  formas=0
  proporcionalidad=propX=prop1=0  #indicarán si se cumplen estas condiciones
  ef=efX=ef1=0  #indicarán si se cumplen estas condiciones
  alfa_prop=alfa_propX=alfa_prop1=0 #los valores máximos que se pueden alcanzar de estos parámetros  
  alfa_ef=alfa_efX=alfa_ef1=0 #los valores máximos que se pueden alcanzar de estos parámetros  
  
  # defino todos los repartos que considero exhaustivos y de base le dan todo al agente 1.
  reparto_prop=reparto_propX=reparto_prop1=vector(mode="list",length=k) #entre los que cumplen la propiedad me quedo con el que maximiza leximin
  reparto_alfa_prop_max=reparto_alfa_prop1_max=reparto_alfa_propX_max=vector(mode="list",length=k)# me quedo con el reparto que maximiza el alfa
  reparto_ef=reparto_efX=reparto_ef1=vector(mode="list",length=k)
  reparto_alfa_ef_max=reparto_alfa_ef1_max=reparto_alfa_efX_max=vector(mode="list",length=k)# me quedo con el reparto que maximiza el alfa
  reparto_max_leximin=reparto_max_nash=reparto_min_envy_ratio=vector(mode="list",length=k) # me quedo con los reparto extremos
  reparto_prop[[1]]=reparto_propX[[1]]=reparto_prop1[[1]]=1:n
  reparto_alfa_prop_max[[1]]=reparto_alfa_prop1_max[[1]]=reparto_alfa_propX_max[[1]]=1:n
  reparto_ef[[1]]=reparto_efX[[1]]=reparto_ef1[[1]]=1:n
  reparto_alfa_ef_max[[1]]=reparto_alfa_ef1_max[[1]]=reparto_alfa_efX_max[[1]]=1:n
  reparto_max_leximin[[1]]=reparto_max_nash[[1]]=reparto_min_envy_ratio[[1]]=1:n

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
        valoracion=valoracionReparto(repartido,valoraciones) #matriz de valoracion 
        aa=envidia2(valoraciones,repartido)
        bb=proporcionalidad(repartido,valoraciones)
        alfa_ef_aux=aa$alfa_ef
        alfa_efX_aux=aa$alfa_efx
        alfa_ef1_aux=aa$alfa_ef1
        envy_ratio_max_aux=1/aa$alfa_ef
        alfa_prop_aux=bb$alfa_prop_min
        alfa_propX_aux=bb$alfa_propX_min
        alfa_prop1_aux=bb$alfa_prop1_min
        
        #actualizo el reparto proporcional
        if(alfa_prop_aux>=1){ #si llega a ser proporcional
          proporcionalidad=1
          if(comparacion_leximin_pp(reparto_prop,repartido,valoraciones)==2){
            reparto_prop=repartido
          }
        }
        #if(alfa_prop_aux>alfa_prop){alfa_prop=alfa_prop_aux}
        #actualizo el reparto propX
        if(alfa_propX_aux>=1){ #si llega a ser propX
          propX=1
          if(comparacion_leximin_pp(reparto_propX,repartido,valoraciones)==2){
            reparto_propX=repartido
          }
        }
        #if(alfa_propX_aux>alfa_propX){alfa_propX=alfa_propX_aux}
        #actualizo reparto prop1
        if(alfa_prop1_aux>=1){ #si llega a ser prop1
          prop1=1
          if(comparacion_leximin_pp(reparto_prop1,repartido,valoraciones)==2){
            reparto_prop1=repartido
          }
        }
        #if(alfa_prop1_aux>alfa_prop1){alfa_prop1=alfa_prop1_aux}
        
        #actualizo repartos ef, efX y ef1
        #actualizo el reparto ef
        if(alfa_ef_aux>=1){ #si llega a ser sin envidia
          ef=1
          if(comparacion_leximin_pp(reparto_ef,repartido,valoraciones)==2){
            reparto_ef=repartido
          }
        }
        #if(alfa_ef_aux>alfa_ef){alfa_ef=alfa_ef_aux}
        #actualizo el reparto efX
        if(alfa_efX_aux>=1){ #si llega a ser efX
          efX=1
          if(comparacion_leximin_pp(reparto_efX,repartido,valoraciones)==2){
            reparto_efX=repartido
          }
        }
        #if(alfa_efX_aux>alfa_efX){alfa_efX=alfa_efX_aux}
        #actualizo reparto ef1
        if(alfa_ef1_aux>=1){ #si llega a ser ef1
          ef1=1
          if(comparacion_leximin_pp(reparto_ef1,repartido,valoraciones)==2){
            reparto_ef1=repartido
          }
        }
        #if(alfa_ef1_aux>alfa_ef1){alfa_ef1=alfa_ef1_aux}
        
        # ahora los repartos que maximiza los alfa_prop y ante empates me quedo con el de mayor orden leximin
        # comienzo con reparto_alfa_ef_max
        if(alfa_ef_aux>alfa_ef){
          reparto_alfa_ef_max=repartido
        }
        if(alfa_ef_aux==alfa_ef){
          if(comparacion_leximin_pp(reparto_alfa_ef_max,repartido,valoraciones)==2){
            reparto_alfa_ef_max=repartido
          }
        }
        if(alfa_ef_aux>alfa_ef){alfa_ef=alfa_ef_aux}
        
        # sigo con reparto_alfa_efX_max
        
        if(alfa_efX_aux>alfa_efX){
          reparto_alfa_efX_max=repartido
        }
        if(alfa_efX_aux==alfa_efX){
          if(comparacion_leximin_pp(reparto_alfa_efX_max,repartido,valoraciones)==2){
            reparto_alfa_efX_max=repartido
          }
        }
        if(alfa_efX_aux>alfa_efX){alfa_efX=alfa_efX_aux}
        
        #continuo con reparto_alfa_ef1_max
        
        
        if(alfa_ef1_aux>alfa_ef1){
          reparto_alfa_ef1_max=repartido
        }
        if(alfa_ef1_aux==alfa_ef1){
          if(comparacion_leximin_pp(reparto_alfa_ef1_max,repartido,valoraciones)==2){
            reparto_alfa_ef1_max=repartido
          }
        }
        if(alfa_ef1_aux>alfa_efX){alfa_ef1=alfa_ef1_aux}
        
        # Ahora todo con reparto_alfa_prop_max reparto_alfa_propX_max y reparto_alfa_prop1_max
        
        # comienzo con reparto_alfa_prop_max
        if(alfa_prop_aux>alfa_prop){
          reparto_alfa_prop_max=repartido
        }
        if(alfa_prop_aux==alfa_prop){
          if(comparacion_leximin_pp(reparto_alfa_prop_max,repartido,valoraciones)==2){
            reparto_alfa_prop_max=repartido
          }
        }
        if(alfa_prop_aux>alfa_prop){alfa_prop=alfa_prop_aux}
        
        # sigo con reparto_alfa_propX_max
        
        if(alfa_propX_aux>alfa_propX){
          reparto_alfa_propX_max=repartido
        }
        if(alfa_propX_aux==alfa_propX){
          if(comparacion_leximin_pp(reparto_alfa_propX_max,repartido,valoraciones)==2){
            reparto_alfa_propX_max=repartido
          }
        }
        if(alfa_propX_aux>alfa_propX){alfa_propX=alfa_propX_aux}
        
        #continuo con reparto_alfa_prop1_max
        
        if(alfa_prop1_aux>alfa_prop1){
          reparto_alfa_prop1_max=repartido
        }
        if(alfa_prop1_aux==alfa_prop1){
          if(comparacion_leximin_pp(reparto_alfa_prop1_max,repartido,valoraciones)==2){
            reparto_alfa_prop1_max=repartido
          }
        }
        if(alfa_prop1_aux>alfa_propX){alfa_prop1=alfa_prop1_aux}
        
        # Ahora falta reparto_max_leximin reparto_max_nash reparto_min_envy_ratio            
        #reparto de maximo leximin
        if(comparacion_leximin_pp(reparto_max_leximin,repartido,valoraciones)==2){
          reparto_max_leximin=repartido
        }
        
        # reparto de Nash
        
        nashAux=productoria(diag(valoracion))$productoSinCeros
        nash_Posit_aux=sum(diag(valoracion)>0)
        
        if(nash_Posit_aux>nash_posit){
          nash_posit=nash_Posit_aux
          reparto_max_nash=repartido
          nashResult=nashAux
        }else{
          if((nash_Posit_aux==nash_posit)&(nashAux>nashResult)){
            reparto_max_nash=repartido
            nashResult=nashAux
          }
        }
        if(envy_ratio_max_aux<envy_ratio_max){
          reparto_min_envy_ratio=repartido
          envy_ratio_max=envy_ratio_max_aux
        }
      }
    }
  }
  valoracion_max_leximin=valoracionReparto(reparto_max_leximin,valoraciones)
  menosLleva=min(diag(valoracion_max_leximin))
  return(list(
    envy_ratio_max=envy_ratio_max,
    nashResult=nashResult,
    nash_posit=nash_posit,
    formas=formas,
    proporcionalidad=proporcionalidad,
    propX=propX,
    prop1=prop1,
    ef=ef,
    efX=efX,
    ef1=ef1,
    alfa_prop=alfa_prop,
    alfa_propX=alfa_propX,
    alfa_prop1=alfa_prop1,
    alfa_ef=alfa_ef,
    alfa_efX=alfa_efX,
    alfa_ef1=alfa_ef1,
    reparto_prop=reparto_prop,
    reparto_propX=reparto_propX,
    reparto_prop1=reparto_prop1,
    reparto_alfa_prop_max=reparto_alfa_prop_max,
    reparto_alfa_prop1_max=reparto_alfa_prop1_max,
    reparto_alfa_propX_max=reparto_alfa_propX_max,
    reparto_ef=reparto_ef,
    reparto_efX=reparto_efX,
    reparto_ef1=reparto_ef1,
    reparto_alfa_ef_max=reparto_alfa_ef_max,
    reparto_alfa_ef1_max=reparto_alfa_ef1_max,
    reparto_alfa_efX_max=reparto_alfa_efX_max,
    reparto_max_leximin=reparto_max_leximin,
    reparto_max_nash=reparto_max_nash,
    reparto_min_envy_ratio=reparto_min_envy_ratio,
    menosLleva_max=menosLleva
    # -----------------------------------------------------------------------------------------
  ))
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


##############################################################################################################
##############################################################################################################
# la funcion aparece_pri tiene entradas:
# "valores" conjunto de valores que están dentro de un vector
# "vector" el vector que contiene a los valores
# y la salida es 
# "valor_pri" el valor de los "valores" que aparece primero en el vector.  
aparece_pri = function(valores,vector){
  #if(setdiff(valores,vector)=!numeric(0)){stop("el conjunto de valores no está contenido en el vector")}
  for(v in vector){
    for(val in valores){
      if(val==v){return(val)}
    }
  }
}


#################
# orden_desempate será un vector que nos dice como desempatar cuando
# hay empate entre los que menos sienten. El primero que aparece
# en el orden es el que elige primero
#################

greedy_alg=function(valoraciones,orden_desempate){
  nAgentes=dim(valoraciones)[2]
  nObjetos=dim(valoraciones)[1]
  props=proporciones(valoraciones)
  props_ord=order(props,decreasing = TRUE)
  reciben=vector(mode="list",length=nAgentes)
  ponderan=rep(0,length=nAgentes) #vector de lo que cada agente pondera su lote
  restantes=1:nObjetos
  for(i in 1:nObjetos){
    ponderan_min=which(ponderan==min(ponderan))
    agente_seleccionado = aparece_pri(ponderan_min,orden_desempate)
    bien_elegido=restantes[which(props[restantes,agente_seleccionado]==max(props[restantes,agente_seleccionado]))[1]]
    reciben[[agente_seleccionado]]=c(reciben[[agente_seleccionado]],bien_elegido)
    ponderan[agente_seleccionado]=ponderan[agente_seleccionado]+props[bien_elegido,agente_seleccionado]
    restantes=setdiff(restantes,bien_elegido)
  }
  return(list(reciben=reciben,ponderan=ponderan))
}

greedy_alg_maximiza_leximin=function(valoraciones){
  n_agentes = dim(valoraciones)[2]
  n_bienes = dim(valoraciones)[1]
  ordenes_posibles=perms(n_agentes)
  cant_ordenes=dim(ordenes_posibles)[2]
  M=proporciones(valoraciones)
  reparto1=vector(mode="list",length=n_agentes)
  reparto1[[1]]=1:n_bienes
  for(i in 1:cant_ordenes){
    orden=ordenes_posibles[,i]
    reparto_aux=greedy_alg(valoraciones,orden)$reciben
    if(comparacion_leximin_pp(reparto1,reparto_aux,valoraciones)==2){
      reparto1=reparto_aux
    }
  }
  reparto1  
}

greedy_alg_rand=function(valoraciones){
  nAgentes=dim(valoraciones)[2]
  nObjetos=dim(valoraciones)[1]
  props=proporciones(valoraciones)
  #props_ord=order(props,decreasing = TRUE)
  reciben=vector(mode="list",length=nAgentes)
  ponderan=rep(0,length=nAgentes) #vector de lo que cada agente pondera su lote
  restantes=1:nObjetos
  for(i in 1:nObjetos){
    ponderan_min=which(ponderan==min(ponderan))
    if(length(ponderan_min)==1){
      agente_seleccionado=ponderan_min
    }else{
      agente_seleccionado=sample(ponderan_min,1)
    }
    lugares_posibles=which(props[restantes,agente_seleccionado]==max(props[restantes,agente_seleccionado]))
    if(length(lugares_posibles)==1){
      bien_elegido=restantes[lugares_posibles]  
    }else{
      bien_elegido=restantes[sample(lugares_posibles,1)]
    }
    
    reciben[[agente_seleccionado]]=c(reciben[[agente_seleccionado]],bien_elegido)
    ponderan[agente_seleccionado]=ponderan[agente_seleccionado]+props[bien_elegido,agente_seleccionado]
    restantes=setdiff(restantes,bien_elegido)
  }
  return(list(reciben=reciben,ponderan=ponderan))
}    



greedy_alg_para2 = function(valoraciones){
  M=proporciones(valoraciones)
  a=M[,1]
  b=M[,2]
  repartido1=list()
  repartido2=list()
  aa=greedy_alg(cbind(a,a),c(1,2))
  bb=greedy_alg(cbind(b,b),c(1,2))
  # primero corta el 1 y elige el 2 y armamos repartido1
  vianda1=sum(M[aa$reciben[[1]],2])
  vianda2=sum(M[aa$reciben[[2]],2])
  if(vianda1>=1/2){
    repartido1[[2]]=aa$reciben[[1]]
    repartido1[[1]]=aa$reciben[[2]]
  }else{
    repartido1[[2]]=aa$reciben[[2]]
    repartido1[[1]]=aa$reciben[[1]]
  }
  # luego corta el 2 y elige el 1 y armamos repartido2
  vianda1=sum(M[bb$reciben[[1]],1])
  vianda2=sum(M[bb$reciben[[2]],1])
  if(vianda1>=1/2){
    repartido2[[1]]=bb$reciben[[1]]
    repartido2[[2]]=bb$reciben[[2]]
  }else{
    repartido2[[1]]=bb$reciben[[2]]
    repartido2[[2]]=bb$reciben[[1]]
  }
  repartido=repartido1
  if(comparacion_leximin_pp(repartido1,repartido2,valoraciones)==2){
    repartido=repartido2
  }
  #finalmente hago el reparto que maximiza el mínimo entre los dos repartos
  # minReparto1=min(sum(M[repartido1[[1]],1]),sum(M[repartido1[[2]],2]))
  # minReparto2=min(sum(M[repartido2[[1]],1]),sum(M[repartido2[[2]],2]))
  # if(minReparto1>=minReparto2){
  #   repartido=repartido1
  # }else{
  #   repartido=repartido2
  # }
  repartido  
}





######
# Habria que programar el reparto leximin para comparar con el resto
# e incluso con alguno que mínimice envy ratio o envidia máxima
######


comparacion_leximin_pp=function(reparto1,reparto2,valoraciones){
  S1=valoracionReparto(reparto1,valoraciones)
  S2=valoracionReparto(reparto2,valoraciones)
  nAgentes=dim(S1)[1]
  valores1=diag(S1) 
  valores2=diag(S2)
  orden1=order(valores1)
  orden2=order(valores2)
  valoresOrd1=valores1[orden1]
  valoresOrd2=valores2[orden2]
  for(i in 1:nAgentes){
    if(valoresOrd1[i]>valoresOrd2[i]){
      gana=1
      return(gana)
    }  
    if(valoresOrd2[i]>valoresOrd1[i]){
      gana=2  
      return(gana)
    }
    if(valoresOrd2[i]==valoresOrd1[i]){
      if(length(reparto1[[orden1[i]]])>length(reparto2[[orden2[i]]])){
        gana=1
        return(gana)
      }
      if(length(reparto2[[orden2[i]]])>length(reparto1[[orden1[i]]])){
        gana=2
        return(gana)
      }
    }
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


grafico_barras=function(nombres, sienten_llevan,llevan_art){
#grafico_barras=function(nombres, sienten_llevan){
  k=length(nombres)
  umbral=1/k*100
  if(min(sienten_llevan)<umbral){
    color_linea="red"
    titulo="No se alcanzó Proporcionalidad
Cuánto sienten que llevan los beneficiarios"}else{
    color_linea="green"
    titulo="Felicitaciones!, hay Proporcionalidad!
Cuánto sienten que llevan los beneficiarios"}
  datos=data.frame(nombres,sienten_llevan,llevan_art)
  grafico <- ggplot(datos, aes(x = nombres, y = sienten_llevan,
                                      text = paste("El",nombres, "siente que lleva", round(sienten_llevan,2),'%',
                                          '<br> Le corresponden los artículos:',sapply(llevan_art, function(x) paste(x, collapse = ","))))) +
#                                                    '<br> lleva artículos:'))) +
    #geom_bar(stat = "identity", fill = "skyblue") +
    geom_bar(position="dodge", stat="identity",fill="skyblue") +
    geom_hline(yintercept = umbral, linetype = "dashed", color = color_linea) +
    labs(title = titulo,
         x = "Beneficiarios",
         y = "Sienten que llevan") +
    theme_minimal()+  # Cambiar el tema a minimal para una apariencia más limpia
    theme(plot.title = element_text(hjust = 0.5))
  # Convertir el gráfico a un gráfico interactivo con plotly
  grafico_interactivo <- ggplotly(grafico, tooltip = "text")

  return(grafico_interactivo)

}


grafico_barras_tareas=function(nombres, sienten_llevan,llevan_art){
  #grafico_barras=function(nombres, sienten_llevan){
  k=length(nombres)
  umbral=1/k*100
  if(max(sienten_llevan)>umbral){
    color_linea="red"
    titulo="No se alcanzó Proporcionalidad
Cuánto sienten que trabajan los Trabajadores"}else{
  color_linea="green"
  titulo="Felicitaciones!, hay Proporcionalidad!
Cuánto sienten que trabajan los Trabajadores"}
  datos=data.frame(nombres,sienten_llevan,llevan_art)
  grafico <- ggplot(datos, aes(x = nombres, y = sienten_llevan,
                               text = paste("El",nombres, "siente que trabaja", round(sienten_llevan,2),'%',
                                            '<br> Le corresponden las tareas:',sapply(llevan_art, function(x) paste(x, collapse = ","))))) +
    #                                                    '<br> Tareas que le tocan:'))) +
    #geom_bar(stat = "identity", fill = "skyblue") +
    geom_bar(position="dodge", stat="identity",fill="skyblue") +
    geom_hline(yintercept = umbral, linetype = "dashed", color = color_linea) +
    labs(title = titulo,
         x = "Trabajadores",
         y = "Sienten que trabajan") +
    theme_minimal()+  # Cambiar el tema a minimal para una apariencia más limpia
    theme(plot.title = element_text(hjust = 0.5))
  # Convertir el gráfico a un gráfico interactivo con plotly
  grafico_interactivo <- ggplotly(grafico, tooltip = "text")
  
  return(grafico_interactivo)
  
}



grafico_barras_envidias=function(nombres, matriz_valoracion){
  #grafico_barras=function(nombres, sienten_llevan){
  maximo_y=max(matriz_valoracion)
  k=dim(matriz_valoracion)[1]
  #par(mfrow=c(1,k))
  grafico_interactivo=vector("list",length=k)
  for(i in 1:k){
    umbral=matriz_valoracion[i,i]
    siente_llevan=matriz_valoracion[i,]
    if(max(siente_llevan)>umbral){
      color_linea="red"
      titulo=paste("El Beneficiario",i,"siente envidia",sep=" ")}else{
      color_linea="green"
      titulo=paste("El Beneficiario",i,"no siente envidia hacia otro beneficiario",sep=" ")}
  


  datos=data.frame(nombres,siente_llevan)
  grafico <- ggplot(datos, aes(x = nombres, y = siente_llevan,
                               text = paste("El",nombres[i], "siente que el",nombres ,"lleva", round(siente_llevan,2),'%'))) +
    #                                                    '<br> lleva artículos:'))) +
    #geom_bar(stat = "identity", fill = "skyblue") +
    geom_bar(position="dodge", stat="identity",fill="skyblue") +
    geom_hline(yintercept = umbral, linetype = "dashed", color = color_linea) +
    labs(title = titulo,
         x = "Beneficiarios",
         y = paste("El",nombres[i],"siente que llevan",sep=" ")) +
    ylim(0,maximo_y)+
    theme(axis.text.x = element_blank())#+
    #theme_minimal()  # Cambiar el tema a minimal para una apariencia más limpia

  # Convertir el gráfico a un gráfico interactivo con plotly
  grafico_interactivo[[i]] =ggplotly(grafico, tooltip = "text") 

  #return(grafico_interactivo)
  }
  graficos_todos=subplot(grafico_interactivo)
  graficos_todos <- graficos_todos %>% layout(title = "Sensaciones de cada Beneficiario")
  graficos_todos
}


grafico_barras_envidias_tareas=function(nombres, matriz_valoracion){
  #grafico_barras=function(nombres, sienten_llevan){
  maximo_y=max(matriz_valoracion)
  k=dim(matriz_valoracion)[1]
  #par(mfrow=c(1,k))
  grafico_interactivo=vector("list",length=k)
  for(i in 1:k){
    umbral=matriz_valoracion[i,i]
    siente_llevan=matriz_valoracion[i,]
    if(min(siente_llevan)<umbral){
      color_linea="red"
      titulo=paste("El trabajador",i,"siente envidia",sep=" ")}else{
        color_linea="green"
        titulo=paste("El trabajador",i,"no siente envidia hacia otro trabajador",sep=" ")}
    
    
    
    datos=data.frame(nombres,siente_llevan)
    grafico <- ggplot(datos, aes(x = nombres, y = siente_llevan,
                                 text = paste("El",nombres[i], "siente que el",nombres ,"trabaja", round(siente_llevan,2),'%'))) +
      #                                                    '<br> lleva artículos:'))) +
      #geom_bar(stat = "identity", fill = "skyblue") +
      geom_bar(position="dodge", stat="identity",fill="skyblue") +
      geom_hline(yintercept = umbral, linetype = "dashed", color = color_linea) +
      labs(title = titulo,
           x = "Trabajadores",
           y = paste("El",nombres[i],"siente que trabaja",sep=" ")) +
      ylim(0,maximo_y)+
      theme(axis.text.x = element_blank())#+
    #theme_minimal()  # Cambiar el tema a minimal para una apariencia más limpia
    
    # Convertir el gráfico a un gráfico interactivo con plotly
    grafico_interactivo[[i]] =ggplotly(grafico, tooltip = "text") 
    
    #return(grafico_interactivo)
  }
#  graficos_todos = subplot(grafico_interactivo, nrows = length(grafico_interactivo), shareX = TRUE, shareY = TRUE)
  graficos_todos=subplot(grafico_interactivo)
  graficos_todos <- graficos_todos %>% layout(title = "Sensaciones de cada Trabajador")
  graficos_todos
}

# 
# 
# 
formatear_vector <- function(vec) {
  if (length(vec) == 1) {
    return(as.character(vec))
  } else {
    elementos <- paste(vec[-length(vec)], collapse = ", ")
    ultimo_elemento <- as.character(vec[length(vec)])
    return(paste(elementos, "y", ultimo_elemento))
  }
}





entregaBien=function(reparto_orig,benef_recibe,benef_entrega,bien_entrega){
  if(!(bien_entrega %in% reparto_orig[[benef_entrega]])){stop("no puede entregar ese bien ese beneficiario")}
  reparto_nuevo=reparto_orig
  reparto_nuevo[[benef_recibe]]=c(reparto_orig[[benef_recibe]],bien_entrega)
  reparto_nuevo[[benef_entrega]]=setdiff(reparto_orig[[benef_entrega]],bien_entrega)
  reparto_nuevo
}

intercambioBien=function(reparto_orig,benef_min,bien_entrega_min,benef_max,bien_entrega_max){
  if(!((bien_entrega_min %in% reparto_orig[[benef_min]]) & (bien_entrega_max %in% reparto_orig[[benef_max]]))){stop("Alguno de los beneficiario no tiene el bien que ofrece entregar")}
  reparto_nuevo=reparto_orig
  reparto_nuevo[[benef_min]]=setdiff(reparto_orig[[benef_min]],bien_entrega_min)
  reparto_nuevo[[benef_min]]=c(reparto_nuevo[[benef_min]],bien_entrega_max)
  reparto_nuevo[[benef_max]]=setdiff(reparto_orig[[benef_max]],bien_entrega_max)
  reparto_nuevo[[benef_max]]=c(reparto_nuevo[[benef_max]],bien_entrega_min)
  reparto_nuevo
}


paso1Agoritmo=function(reparto_orig,matriz_valoracion){   #le intentamos dar un bien a alguno de los que sienten que se llevan menos a ver si mejora el leximin
  cambio="no"
  valoran_reparto_mat=valoracionReparto(reparto_orig,matriz_valoracion)
  valoran_reparto_vec=diag(valoran_reparto_mat)
  valoran_menor_a_mayor=order(valoran_reparto_vec)
  valoran_min=which(valoran_reparto_vec==min(valoran_reparto_vec)) #aquellos que sienten que reciben lo mínimo
  Props=proporciones(matriz_valoracion)
  n_art=dim(Props)[1] #cantidad de artículos
  n_benef=dim(Props)[2] #cantidad de beneficiarios
  dif=array(,c(n_benef,n_benef,n_art)) #las diferencias e/ proporciones
  for(i in 1:n_benef){
    for(j in 1:n_benef){
      dif[i,j,]=Props[,i]-Props[,j]    
    }
  }
  #difAbs=abs(dif)
  #for(i in valoran_min){
  for(i in valoran_menor_a_mayor){
    #diferencias=difAbs[i,-i,]
    diferencias=dif[i,-i,]
    #diferencias_ord=sort(unique(difAbs[i,-i,]))
    diferencias_ord=sort(unique(dif[i,-i,]),decreasing = TRUE) #pongo primero los artículos que más valora i en relación a otres
    largo_dif=length(diferencias)
    for(j in 1:largo_dif){
      posiciones=which(diferencias==diferencias_ord[j],arr.ind = T) #busco los lugares de la matriz para saber
      if(n_benef==2){
        #largo_pos=length(posiciones)
        for(k in posiciones){
          #if (!(k %in% reparto_orig[[i]])){
          if (k %in% reparto_orig[[((1:n_benef)[-i])]]){
            reparto_nuevo=entregaBien(reparto_orig,i,((1:n_benef)[-i]),k)
            if(comparacion_leximin_pp(reparto_orig,reparto_nuevo,matriz_valoracion)==2){
              cambio="si"
              return(list(reparto_nuevo=reparto_nuevo,cambio=cambio))
            }  
          }
        }
      }
      if(n_benef>2){
        largo_pos=dim(posiciones)[1]
        for(k in 1:largo_pos){
          #if (!(posiciones[k,2] %in% reparto_orig[[i]])){ qué es posiciones[k,2]???? es el artículo donde se cumple la diferencia k-ésima
          if (posiciones[k,2] %in% reparto_orig[[((1:n_benef)[-i])[posiciones[k,1]]]]){ #si justo ese artículo lo tiene la persona con la que tiene esa diferencia, intentamos dárselo
            reparto_nuevo=entregaBien(reparto_orig,i,((1:n_benef)[-i])[posiciones[k,1]],as.numeric(posiciones[k,2]))
            if(comparacion_leximin_pp(reparto_orig,reparto_nuevo,matriz_valoracion)==2){
              cambio="si"
              return(list(reparto_nuevo=reparto_nuevo,cambio=cambio))
            }
          }  
        }
      }
    }
  }
  return(list(reparto_nuevo=reparto_orig,cambio=cambio))
}
  
# la función repartoBienes según la matriz de valoración, 
# comienza con un reparto al azar de los bienes y luego les va intentando dar bienes
# a los que menos sienten que llevan. El bien que les intenta dar es el que más mejoraría
# la utilidad y siempre que no empeore el leximinpp.
repartoBienes=function(n_hered,matriz_valoracion){
  M=matriz_valoracion
  n_bienes=dim(M)[1]
  if(n_hered != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  
  Props=proporciones(M)
  
  art=vector("list",n_hered)
  
  for(i in 1:n_bienes){
    #maximizan=which(Props[i,]==max(Props[i,]))
    #n_max=length(maximizan)
    #j=maximizan[sample(n_max)]
    j=sample(n_hered,1)
    art[[j]]=c(art[[j]],i)
  }
  
  reparto_orig=art
  cambio="si"
  while(cambio=="si"){
    repartoAux=paso1Agoritmo(reparto_orig,M)
    reparto_orig=repartoAux$reparto_nuevo
    cambio=repartoAux$cambio
  }
  ###################################
  # Falta agregar el paso2Algoritmo, que podría estar relacionado con intercambio de bienes.
  ###################################
  
  lleva=diag(valoracionReparto(reparto_orig,matriz_valoracion))
  
  return(list(Art=reparto_orig,llevan=lleva))
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



tareasAQuien=function(reparto){
  n_trab=length(reparto)
  n_tareas=sum(lengths(reparto))
  asignoTareas=vector(,n_tareas)
  for(i in 1:n_trab){
    asignoTareas[reparto[[i]]]=i
  }
  asignoTareas
}

# Función para detectar ciclos en un grafo dirigido y devolver uno de los ciclos encontrados
detectar_ciclos <- function(grafo) {
  n <- vcount(grafo)
  visitado <- rep(0, n)
  ciclo <- NULL
  
  for (i in 1:n) {
    if (visitado[i] == 0) {
      ciclo <- dfs_ciclo(grafo, i, visitado, c(i))
      if (!is.null(ciclo)) {
        return(ciclo)
      }
    }
  }
  
  return(NULL)
}

# Función auxiliar para búsqueda en profundidad (DFS) para detectar ciclos
dfs_ciclo <- function(grafo, v, visitado, camino) {
  visitado[v] <- 1
  
  adyacentes <- neighbors(grafo, v, mode = "out")
  for (w in adyacentes) {
    if (w == camino[1]) {
      # Hemos encontrado un ciclo
      return(c(camino, w))
    } else if (visitado[w] == 0) {
      ciclo <- dfs_ciclo(grafo, w, visitado, c(camino, w))
      if (!is.null(ciclo)) {
        return(ciclo)
      }
    }
  }
  visitado[v] <- 2
  return(NULL)
}

grafo_envidias = function(reparto,valoraciones){
  S=valoracionReparto(reparto,valoraciones)
  agentes_cant=dim(valoraciones)[2]
  aristas=c()
  lista_envidiados=vector(mode="list",length=agentes_cant)
  for(i in 1:agentes_cant){
    envidiados=which(S[i,]>S[i,i])
    largo=length(envidiados)
    if(largo>0){
      for(j in 1:largo){
        aristas=c(aristas,i,envidiados[j])
      }
    }
  }
  grafo_respuesta=graph(edges=aristas)
}

cant_envidiados = function(grafo_de_evidias){
  in_degree = degree(grafo_de_evidias,mode="in")
  cant=sum(in_degree>0)
  cant
}

# este método de Lipton, sortea entre los no envidiados a quien le reparte el próximo bien
# que aún no haya sido repartido. Un vez elegida la persona, le entrega el bien que más
# valora entre los que aún no se repartieron. Luego se fija si hay ciclos de envidia y los elimina
# y procede como antes. Realizar varias veces este reparto para la misma instancia, e ir quedándonos
# con el que más nos gusta, ya sea comparando leximin, o minimizando el envyratio o maximizando el bienestar de Nash,etc

envy_cycle_elimination_mejor = function(agents,partial_alloc,unalloc_goods,valoraciones){
  cant_agentes=length(agents)
  grafoDeEnvidias=grafo_envidias(partial_alloc,valoraciones)
  unalloc_goods_orden=sort(unalloc_goods)
  cant_bienes=length(unalloc_goods)#nuevo
  #for(g in unalloc_goods_orden){
  for(g in 1:cant_bienes){ #nuevo
    while(cant_envidiados(grafoDeEnvidias)==cant_agentes){
      ciclo=detectar_ciclos(grafoDeEnvidias)
      largo_ciclo=length(ciclo)-1
      B=partial_alloc[[ciclo[1]]]
      for(k in 1:(largo_ciclo-1)){
        partial_alloc[[ciclo[k]]]=partial_alloc[[ciclo[k+1]]]
      }
      partial_alloc[[ciclo[largo_ciclo]]]=B
      grafoDeEnvidias=grafo_envidias(partial_alloc,valoraciones)
    }
    vertices=V(grafoDeEnvidias)
    if(length(vertices)>0){
      in_degree=degree(grafoDeEnvidias,mode="in")
      envidiados_lugares=which(in_degree>0)
      envidiados=vertices[envidiados_lugares]
      no_envidiados=setdiff(agents,envidiados)
    }else{
      no_envidiados=agents
    }
    #elegido_agente=no_envidiados[1]
    if(length(no_envidiados)>1){
      elegido_agente=sample(no_envidiados,1)
    }else{
      elegido_agente=no_envidiados
    }
    bien_mas_valorado_lugar=which(valoraciones[unalloc_goods_orden,elegido_agente]==max(valoraciones[unalloc_goods_orden,elegido_agente]))[1]#nuevo
    bien_mas_valorado=unalloc_goods_orden[bien_mas_valorado_lugar] #nuevo
    partial_alloc[[elegido_agente]]=c(partial_alloc[[elegido_agente]],bien_mas_valorado) #nuevo
    unalloc_goods_orden=unalloc_goods_orden[-bien_mas_valorado_lugar]#nuevo
    grafoDeEnvidias=grafo_envidias(partial_alloc,valoraciones)    
    
  }
  partial_alloc
}
# ver si mejora eligiendo al azar a quien darle entre los no envidiosos y probando varias veces e ir eligiendo
# la que mejore por ejemplo el leximinpp o el máximo envyratio.



Round_Robin = function(agentes,partial_alloc,unalloc_goods,orden,pasos,valoraciones){
  k=1
  nAgentes=length(agentes)
  while((length(unalloc_goods)>0)&(pasos>0)){
    le_toca = k%%nAgentes
    if(le_toca==0){le_toca=nAgentes}
    g_lugar=which(valoraciones[unalloc_goods,orden[le_toca]]==max(valoraciones[unalloc_goods,orden[le_toca]]))
    if(length(g_lugar)>1){
      g=unalloc_goods[sample(g_lugar,1)]  
    }else{
      g=unalloc_goods[g_lugar]
    }
    partial_alloc[[orden[le_toca]]]=c(partial_alloc[[orden[le_toca]]],g)
    unalloc_goods=setdiff(unalloc_goods,g)
    k=k+1
    pasos=pasos-1
  }
  return(list(partial_alloc=partial_alloc,unalloc_goods=unalloc_goods))
}


####
# Método de Plaut-Rauphgarden cuando son dos personas
####
# el método consiste en que uno de los agentes  divide los objetos en
# dos conjuntos buscando el reparto con leximinpp máximo si dos personas 
# valoraran como él. Luego el otro elige, usando su propia valoración,
# cuál de los dos conjuntos se lleva.

# valoraciones es la matriz de dos columnas: cada una con las valoraciones de c/u
plaut_rouph_2agentes = function(valoraciones){
  if(dim(valoraciones)[2]!=2){stop("el metodo sólo sirve para dos agentes")}
  n_bienes=dim(valoraciones)[1]
  valoraciones1=valoraciones[,1]
  valoraciones_aux=cbind(valoraciones1,valoraciones1)
  reparto1=repartoExhaustivo(n_bienes,2,valoraciones_aux)$reparto_leximin_opt
  # ahora lo hago elegir a 2
  reparto=reparto1 
  # en principio la dejo igual, pero si el 2 prefiere el lote del 1 se la doy
  if(sum(valoraciones[reparto1[[1]],2])>sum(valoraciones[reparto1[[2]],2])){
    reparto[[2]]=reparto1[[1]]
    reparto[[1]]=reparto1[[2]]
  }
  reparto  
}

# la función "plaut_roupd_3omas" resulta EFX cuando las valuaciones son idénticas
plaut_rouph_3omas = function(valoraciones){
  n_bienes=dim(valoraciones)[1]
  n_agentes=dim(valoraciones)[2]
  reparto=repartoExhaustivo(n_bienes,n_agentes,valoraciones)$reparto_leximin_opt
  reparto
}


simulacion= function(nRep1i,nRep1f,nRep2,nRep3,nObjetos,nAgentes,lambda){
  #nRep1=10
  ef1_lipton=ef1_mio=ef1_plaut=ef1_exhEFX2=ef1_exhNash=ef1_robin=ef1_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  efx_lipton=efx_mio=efx_plaut=efx_exhEFX2=efx_exhNash=efx_robin=efx_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  envidiaMaxima_lipton=envidiaMaxima_mio=envidiaMaxima_plaut=envidiaMaxima_exhEFX2=envidiaMaxima_exhNash=envidiaMaxima_robin=envidiaMaxima_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  envidiosos_lipton=envidiosos_mio=envidiosos_plaut=envidiosos_exhEFX2=envidiosos_exhNash=envidiosos_robin=envidiosos_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  proporcionalidad_lipton=proporcionalidad_mio=proporcionalidad_plaut=proporcionalidad_exhEFX2=proporcionalidad_exhNash=proporcionalidad_robin=proporcionalidad_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  bienSocial_lipton=bienSocial_mio=bienSocial_plaut=bienSocial_exhEFX2=bienSocial_exhNash=bienSocial_robin=bienSocial_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  bienNash_lipton=bienNash_mio=bienNash_plaut=bienNash_exhEFX2=bienNash_exhNash=bienNash_robin=bienNash_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  menosLleva_lipton=menosLleva_mio=menosLleva_plaut=menosLleva_exhEFX2=menosLleva_exhNash=menosLleva_robin=menosLleva_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  esProporcional=vector(,length=(nRep1f-nRep1i+1))
  #vector(,length=nRep1)
  #dif=dif2=vector(,length=nRep1)
  #nObjetos=6
  #nAgentes=3
  #nRep2=100
  #porcDiscrep=0.3
  alfaVec=rep(1,nObjetos)
  #lambda=100
  nombre_archi=paste("reparto_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  if(nAgentes==2){
    plaut_func=plaut_rouph_2agentes
  }else{
    plaut_func=plaut_rouph_3omas
  }
  
  archivo_resultados <- nombre_archi
  
  ef1_nombres=c("ef1_lip","ef1_mio","ef1_pla","ef1_EFX","ef1_nas","ef1_rob","ef1_sol")
  efx_nombres=c("efx_lip","efx_mio","efx_pla","efx_EFX","efx_nas","efx_rob","efx_sol")
  enviMax_nombres=c("enviMax_lip","enviMax_mio","enviMax_pla","enviMax_EFX","enviMax_nas","enviMax_rob","enviMax_sol")
  envis_nombres=c("envis_lip","envis_mio","envis_pla","envis_EFX","envis_nas","envis_rob","envis_sol")
  prop_nombres=c("prop_lip","prop_mio","prop_pla","prop_EFX","prop_nas","prop_rob","prop_sol")
  nash_nombres=c("nash_lip","nash_mio","nash_pla","nash_EFX","nash_nas","nash_rob","nash_sol")
  social_nombres=c("social_lip","social_mio","social_pla","social_EFX","social_rob","social_nas","social_sol") #ojo que estan invertidos social_rob y social_nas
  menosLleva_nombres=c("menosLleva_lip","menosLleva_mio","menosLleva_pla","menosLleva_EFX","menosLleva_nas","menosLleva_rob","menosLleva_sol")
  esProporcional_nombres=c("proporcional")
  
  columnas=c("rep","caso",ef1_nombres,efx_nombres,enviMax_nombres,envis_nombres,prop_nombres,nash_nombres,social_nombres,menosLleva_nombres,esProporcional_nombres)
  write.table(t(columnas), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  #X1 <- rdirichlet(nRep1, alfaVec)
  for(i in nRep1i:nRep1f){
    set.seed(500+i)
    X1 <- rdirichlet(1, alfaVec)
    # M=matrix(,nObjetos,nAgentes)
    for(j in 1:nRep2){
      
      M <- t(rdirichlet(nAgentes,lambda*(as.vector(X1))))

      # reparto mío  
      reparto_mio=vector("list",length=nAgentes)
      reparto_mio[[1]]=1:nObjetos
      for(l in 1:nRep3){
        reparto_aux=repartoBienes(nAgentes,M)$Art
        if(comparacion_leximin_pp(reparto_mio,reparto_aux,M)==2){
          reparto_mio=reparto_aux
        }
      }
      
      # reparto lipton  
      reparto_lipton=vector("list",length=nAgentes)
      reparto_lipton[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
        if(comparacion_leximin_pp(reparto_lipton,repartido_aux,M)==2){
          reparto_lipton=repartido_aux
        }
      }
      
      # reparto Plat Rouphgarden  
      
      plaut=felicidad2(plaut_func(M),M)
      
      #reparto exhaustivo que busca asignacion EF dentro de asignaciones EFX ¿qué pasa si no hay EFX?
      exhEFX2=felicidad2(repartoExhaustivoEFX2(nObjetos,nAgentes,M)$repartido,M)
      
      #reparto exhaustivo que maximiza el bienestar de Nash
      exhNash=felicidad2(repartoExhaustivoEFX2(nObjetos,nAgentes,M)$repartidoNash,M)
      
      lipton=felicidad2(reparto_lipton,M)
      mio=felicidad2(reparto_mio,M)
      
      # reparto lipton solo. No hace mini simulacion para buscar el mejor.  
      reparto_lipton_solo=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
      lipton_solo=felicidad2(reparto_lipton_solo,M)
    
      # reparto usando Round_robin salteador
      
      repartido_orig=vector("list",length=nAgentes)
      repartido_orig[[1]]=1:nObjetos
      for(l in 1:nRep2){
        orden=sample(1:nAgentes,nAgentes)
        repartido_aux=Round_Robin_salteador(1:nAgentes,vector("list",nAgentes),1:nObjetos,orden,nObjetos,M)$partial_alloc
        if(comparacion_leximin_pp(repartido_orig,repartido_aux,M)==2){
          repartido_orig=repartido_aux
        }
      }
      robin=felicidad2(repartido_orig,M)
      
      
      ef1_lipton[i]=lipton$ef1[[1]]
      ef1_mio[i]=mio$ef1[[1]]
      ef1_plaut[i]=plaut$ef1[[1]]
      ef1_exhEFX2[i]=exhEFX2$ef1[[1]]
      ef1_exhNash[i]=exhNash$ef1[[1]]
      ef1_robin[i]=robin$ef1[[1]]
      ef1_lipton_solo[i]=lipton_solo$ef1[[1]]
      
      efx_lipton[i]=lipton$efx[[1]]
      efx_mio[i]=mio$efx[[1]]
      efx_plaut[i]=plaut$efx[[1]]
      efx_exhEFX2[i]=exhEFX2$efx[[1]]
      efx_exhNash[i]=exhNash$efx[[1]]
      efx_robin[i]=robin$efx[[1]]
      efx_lipton_solo[i]=lipton_solo$efx[[1]]
      
      envidiaMaxima_lipton[i]=max(lipton$envidiaMaxima)
      envidiaMaxima_mio[i]=max(mio$envidiaMaxima)
      envidiaMaxima_plaut[i]=max(plaut$envidiaMaxima)
      envidiaMaxima_exhEFX2[i]=max(exhEFX2$envidiaMaxima)
      envidiaMaxima_exhNash[i]=max(exhNash$envidiaMaxima)
      envidiaMaxima_robin[i]=max(robin$envidiaMaxima)
      envidiaMaxima_lipton_solo[i]=max(lipton_solo$envidiaMaxima)
      
      envidiosos_lipton[i]=lipton$envidiosos
      envidiosos_mio[i]=mio$envidiosos
      envidiosos_plaut[i]=plaut$envidiosos
      envidiosos_exhEFX2[i]=exhEFX2$envidiosos
      envidiosos_exhNash[i]=exhNash$envidiosos
      envidiosos_robin[i]=robin$envidiosos
      envidiosos_lipton_solo[i]=lipton_solo$envidiosos
      
      proporcionalidad_lipton[i]=lipton$proporcionalidad
      proporcionalidad_mio[i]=mio$proporcionalidad
      proporcionalidad_plaut[i]=plaut$proporcionalidad
      proporcionalidad_exhEFX2[i]=exhEFX2$proporcionalidad
      proporcionalidad_exhNash[i]=exhNash$proporcionalidad
      proporcionalidad_robin[i]=robin$proporcionalidad
      proporcionalidad_lipton_solo[i]=lipton_solo$proporcionalidad
      
      bienNash_lipton[i]=lipton$bienNash
      bienNash_mio[i]=mio$bienNash
      bienNash_plaut[i]=plaut$bienNash
      bienNash_exhEFX2[i]=exhEFX2$bienNash
      bienNash_exhNash[i]=exhNash$bienNash
      bienNash_robin[i]=robin$bienNash
      bienNash_lipton_solo[i]=lipton_solo$bienNash
      
      bienSocial_lipton[i]=lipton$bienSocial
      bienSocial_mio[i]=mio$bienSocial
      bienSocial_plaut[i]=plaut$bienSocial
      bienSocial_exhEFX2[i]=exhEFX2$bienSocial
      bienSocial_exhNash[i]=exhNash$bienSocial
      bienSocial_robin[i]=robin$bienSocial
      bienSocial_lipton_solo[i]=lipton_solo$bienSocial
      
      menosLleva_lipton[i]=lipton$menosLleva
      menosLleva_mio[i]=mio$menosLleva
      menosLleva_plaut[i]=plaut$menosLleva
      menosLleva_exhEFX2[i]=exhEFX2$menosLleva
      menosLleva_exhNash[i]=exhNash$menosLleva
      menosLleva_robin[i]=robin$menosLleva
      menosLleva_lipton_solo[i]=lipton_solo$menosLleva
      
      esProporcional[i]=exhEFX2$proporcionalidad
      
      ef1s=c(ef1_lipton[i],ef1_mio[i],ef1_plaut[i],ef1_exhEFX2[i],ef1_exhNash[i],ef1_robin[i],ef1_lipton_solo[i])
      efxs=c(efx_lipton[i],efx_mio[i],efx_plaut[i],efx_exhEFX2[i],efx_exhNash[i],efx_robin[i],efx_lipton_solo[i])
      enviMaxs=c(envidiaMaxima_lipton[i],envidiaMaxima_mio[i],envidiaMaxima_plaut[i],envidiaMaxima_exhEFX2[i],envidiaMaxima_exhNash[i],envidiaMaxima_robin[i],envidiaMaxima_lipton_solo[i])
      envis=c(envidiosos_lipton[i],envidiosos_mio[i],envidiosos_plaut[i],envidiosos_exhEFX2[i],envidiosos_exhNash[i],envidiosos_robin[i],envidiosos_lipton_solo[i])
      props=c(proporcionalidad_lipton[i],proporcionalidad_mio[i],proporcionalidad_plaut[i],proporcionalidad_exhEFX2[i],proporcionalidad_exhNash[i],proporcionalidad_robin[i],proporcionalidad_lipton_solo[i])
      nashs=c(bienNash_lipton[i],bienNash_mio[i],bienNash_plaut[i],bienNash_exhEFX2[i],bienNash_exhNash[i],bienNash_robin[i],bienNash_lipton_solo[i])
      socials=c(bienSocial_lipton[i],bienSocial_mio[i],bienSocial_plaut[i],bienSocial_exhEFX2[i],bienSocial_exhNash[i],bienSocial_robin[i],bienSocial_lipton_solo[i])
      menosLlevan=c(menosLleva_lipton[i],menosLleva_mio[i],menosLleva_plaut[i],menosLleva_exhEFX2[i],menosLleva_exhNash[i],menosLleva_robin[i],menosLleva_lipton_solo[i])
      
      guardo=c(i,j,ef1s,efxs,enviMaxs,envis,props,nashs,socials,menosLlevan,esProporcional[i])
      
      write.table(t(guardo), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      print(paste("repeticion", i, "_caso_", j,sep=""))  
    }
  }  
}

#ideas nuevas 2024-08-12
# hacer un round robin donde 
# a partir de la segunda ronda, si alguien
# no envidia a nadie está obligado a decir paso y que elija el que sigue.
# Eso sería EF1, además podemos probar con sortear órdenes varias veces
# y quedarnos con el reparto que minimiza el max envy ratio o que 
# maximiza el leximin


Round_Robin_salteador = function(agentes,partial_alloc,unalloc_goods,orden,pasos,valoraciones){
  k=1
  nAgentes=length(agentes)
  salteos=0
  while((length(unalloc_goods)>0)&(pasos>0)){
    le_toca = k%%nAgentes
    if(le_toca==0){le_toca=length(agentes)}
    if((envidia(valoraciones,partial_alloc)$envidian[orden[le_toca]]>0)||(k==1)||(salteos==nAgentes)){
      g_lugar=which(valoraciones[unalloc_goods,orden[le_toca]]==max(valoraciones[unalloc_goods,orden[le_toca]]))
      if(length(g_lugar)>1){
        g=unalloc_goods[sample(g_lugar,1)]  
      }else{
        g=unalloc_goods[g_lugar]
      }
      partial_alloc[[orden[le_toca]]]=c(partial_alloc[[orden[le_toca]]],g)
      unalloc_goods=setdiff(unalloc_goods,g)
      pasos=pasos-1
      salteos=0
    }else{
      salteos=salteos+1
    }
    k=k+1
  }
  return(list(partial_alloc=partial_alloc,unalloc_goods=unalloc_goods))
}


Round_Robin_salteador_maximiza_leximin = function(valoraciones){
  n_agentes = dim(valoraciones)[2]
  n_bienes = dim(valoraciones)[1]
  ordenes_posibles=perms(n_agentes)
  cant_ordenes=dim(ordenes_posibles)[2]
  M=proporciones(valoraciones)
  reparto1=vector(mode="list",length=n_agentes)
  reparto1[[1]]=1:n_bienes
  for(i in 1:cant_ordenes){
    orden=ordenes_posibles[,i]
    reparto_aux=Round_Robin_salteador(1:n_agentes,vector("list",n_agentes),1:n_bienes,orden,n_bienes,M)$partial_alloc
    if(comparacion_leximin_pp(reparto1,reparto_aux,valoraciones)==2){
      reparto1=reparto_aux
    }
  }
  reparto1  
}

###########
# ahora la simulacion cuando no corro repartos exhaustivos
###########

simulacionSinExh = function(nRep1i,nRep1f,nRep2,nRep3,nObjetos,nAgentes,lambda){
  #nRep1=10
  alfa_ef_mio=alfa_ef_lipton=alfa_ef_lipton_solo=alfa_ef_greedy_solo=alfa_ef_robin_solo=alfa_ef_robin_salt_solo=alfa_ef_greedy_glob=alfa_ef_robin_glob=alfa_ef_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_ef1_mio=alfa_ef1_lipton=alfa_ef1_lipton_solo=alfa_ef1_greedy_solo=alfa_ef1_robin_solo=alfa_ef1_robin_salt_solo=alfa_ef1_greedy_glob=alfa_ef1_robin_glob=alfa_ef1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_efx_mio=alfa_efx_lipton=alfa_efx_lipton_solo=alfa_efx_greedy_solo=alfa_efx_robin_solo=alfa_efx_robin_salt_solo=alfa_efx_greedy_glob=alfa_efx_robin_glob=alfa_efx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_prop_mio=alfa_prop_lipton=alfa_prop_lipton_solo=alfa_prop_greedy_solo=alfa_prop_robin_solo=alfa_prop_robin_salt_solo=alfa_prop_greedy_glob=alfa_prop_robin_glob=alfa_prop_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_prop1_mio=alfa_prop1_lipton=alfa_prop1_lipton_solo=alfa_prop1_greedy_solo=alfa_prop1_robin_solo=alfa_prop1_robin_salt_solo=alfa_prop1_greedy_glob=alfa_prop1_robin_glob=alfa_prop1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_propx_mio=alfa_propx_lipton=alfa_propx_lipton_solo=alfa_propx_greedy_solo=alfa_propx_robin_solo=alfa_propx_robin_salt_solo=alfa_propx_greedy_glob=alfa_propx_robin_glob=alfa_propx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  ef_mio=ef_lipton=ef_lipton_solo=ef_greedy_solo=ef_robin_solo=ef_robin_salt_solo=ef_greedy_glob=ef_robin_glob=ef_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  ef1_mio=ef1_lipton=ef1_lipton_solo=ef1_greedy_solo=ef1_robin_solo=ef1_robin_salt_solo=ef1_greedy_glob=ef1_robin_glob=ef1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  efx_mio=efx_lipton=efx_lipton_solo=efx_greedy_solo=efx_robin_solo=efx_robin_salt_solo=efx_greedy_glob=efx_robin_glob=efx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  envidiaMaxima_mio=envidiaMaxima_lipton=envidiaMaxima_lipton_solo=envidiaMaxima_greedy_solo=envidiaMaxima_robin_solo=envidiaMaxima_robin_salt_solo=envidiaMaxima_greedy_glob=envidiaMaxima_robin_glob=envidiaMaxima_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  envidiosos_mio=envidiosos_lipton=envidiosos_lipton_solo=envidiosos_greedy_solo=envidiosos_robin_solo=envidiosos_robin_salt_solo=envidiosos_greedy_glob=envidiosos_robin_glob=envidiosos_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  proporcionalidad_mio=proporcionalidad_lipton=proporcionalidad_lipton_solo=proporcionalidad_greedy_solo=proporcionalidad_robin_solo=proporcionalidad_robin_salt_solo=proporcionalidad_greedy_glob=proporcionalidad_robin_glob=proporcionalidad_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  bienSocial_mio=bienSocial_lipton=bienSocial_lipton_solo=bienSocial_greedy_solo=bienSocial_robin_solo=bienSocial_robin_salt_solo=bienSocial_greedy_glob=bienSocial_robin_glob=bienSocial_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  bienNash_mio=bienNash_lipton=bienNash_lipton_solo=bienNash_greedy_solo=bienNash_robin_solo=bienNash_robin_salt_solo=bienNash_greedy_glob=bienNash_robin_glob=bienNash_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  menosLleva_mio=menosLleva_lipton=menosLleva_lipton_solo=menosLleva_greedy_solo=menosLleva_robin_solo=menosLleva_robin_salt_solo=menosLleva_greedy_glob=menosLleva_robin_glob=menosLleva_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  esProporcionalEst=vector(,length=(nRep1f-nRep1i+1)) #ahora es una estimación, si encuentra algún reparto proporcional da 1
  tiempo_mio=tiempo_lip=tiempo_lip_solo=tiempo_greedy_solo=tiempo_robin_solo=tiempo_robin_salt_solo=tiempo_greedy_glob=tiempo_robin_glob=tiempo_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  valores_c_simul=read.table("valores_c_Dirichlet.txt",header=TRUE)
  caso=intersect(which(round(valores_c_simul$E,3)==round(1/nAgentes,3)),which(valores_c_simul$k==nObjetos))
  valor_c=valores_c_simul$c[caso]
  alfaVec=rep(valor_c,nObjetos)
  nombre_archi=paste("reparto_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  nombre_archi_inst=paste("instancia_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  archivo_resultados <- nombre_archi
  archivo_instancia <- nombre_archi_inst
  
  alfa_ef_nombres=c("alfa_ef_mio","alfa_ef_lip_sol","alfa_ef_lip","alfa_ef_gre_sol","alfa_ef_rob_sol","alfa_ef_rob_salt_sol","alfa_ef_gre_glo","alfa_ef_rob_glo","alfa_ef_gre_ale")
  alfa_ef1_nombres=c("alfa_ef1_mio","alfa_ef1_lip_sol","alfa_ef1_lip","alfa_ef1_gre_sol","alfa_ef1_rob_sol","alfa_ef1_rob_salt_sol","alfa_ef1_gre_glo","alfa_ef1_rob_glo","alfa_ef1_gre_ale")
  alfa_efx_nombres=c("alfa_efx_mio","alfa_efx_lip_sol","alfa_efx_lip","alfa_efx_gre_sol","alfa_efx_rob_sol","alfa_efx_rob_salt_sol","alfa_efx_gre_glo","alfa_efx_rob_glo","alfa_efx_gre_ale")
  alfa_prop_nombres=c("alfa_prop_mio","alfa_prop_lip_sol","alfa_prop_lip","alfa_prop_gre_sol","alfa_prop_rob_sol","alfa_prop_rob_salt_sol","alfa_prop_gre_glo","alfa_prop_rob_glo","alfa_prop_gre_ale")
  alfa_prop1_nombres=c("alfa_prop1_mio","alfa_prop1_lip_sol","alfa_prop1_lip","alfa_prop1_gre_sol","alfa_prop1_rob_sol","alfa_prop1_rob_salt_sol","alfa_prop1_gre_glo","alfa_prop1_rob_glo","alfa_prop1_gre_ale")
  alfa_propx_nombres=c("alfa_propx_mio","alfa_propx_lip_sol","alfa_propx_lip","alfa_propx_gre_sol","alfa_propx_rob_sol","alfa_propx_rob_salt_sol","alfa_propx_gre_glo","alfa_propx_rob_glo","alfa_propx_gre_ale")
  ef_nombres=c("ef_mio","ef_lip_sol","ef_lip","ef_gre_sol","ef_rob_sol","ef_rob_salt_sol","ef_gre_glo","ef_rob_glo","ef_gre_ale")
  ef1_nombres=c("ef1_mio","ef1_lip_sol","ef1_lip","ef1_gre_sol","ef1_rob_sol","ef1_rob_salt_sol","ef1_gre_glo","ef1_rob_glo","ef1_gre_ale")
  efx_nombres=c("efx_mio","efx_lip_sol","efx_lip","efx_gre_sol","efx_rob_sol","efx_rob_salt_sol","efx_gre_glo","efx_rob_glo","efx_gre_ale")
  envyMax_nombres=c("envyMax_mio","envyMax_lip_sol","envyMax_lip","envyMax_gre_sol","envyMax_rob_sol","envyMax_rob_salt_sol","envyMax_gre_glo","envyMax_rob_glo","envyMax_gre_ale")
  envis_nombres=c("envis_mio","envis_lip_sol","envis_lip","envis_gre_sol","envis_rob_sol","envis_rob_salt_sol","envis_gre_glo","envis_rob_glo","envis_gre_ale")
  prop_nombres=c("prop_mio","prop_lip_sol","prop_lip","prop_gre_sol","prop_rob_sol","prop_rob_salt_sol","prop_gre_glo","prop_rob_glo","prop_gre_ale")
  nash_nombres=c("nash_mio","nash_lip_sol","nash_lip","nash_gre_sol","nash_rob_sol","nash_rob_salt_sol","nash_gre_glo","nash_rob_glo","nash_gre_ale")
  social_nombres=c("social_mio","social_lip_sol","social_lip","social_gre_sol","social_rob_sol","social_rob_salt_sol","social_gre_glo","social_rob_glo","social_gre_ale")
  menosLleva_nombres=c("menosLleva_mio","menosLleva_lip_sol","menosLleva_lip","menosLleva_gre_sol","menosLleva_rob_sol","menosLleva_rob_salt_sol","menosLleva_gre_glo","menosLleva_rob_glo","menosLleva_gre_ale")
  esProporcional_nombres=c("proporcional")
  tiempo_nombres=c("tiempo_mio","tiempo_lip_sol","tiempo_lip","tiempo_gre_sol","tiempo_rob_sol","tiempo_rob_salt_sol","tiempo_gre_glo","tiempo_rob_glo","tiempo_gre_ale")
  columnas=c("rep","caso",alfa_ef_nombres,alfa_ef1_nombres,alfa_efx_nombres,alfa_prop_nombres,alfa_prop1_nombres,alfa_propx_nombres,ef_nombres,ef1_nombres,efx_nombres,envyMax_nombres,envis_nombres,prop_nombres,nash_nombres,social_nombres,menosLleva_nombres,esProporcional_nombres,tiempo_nombres)
  columnas2=c("rep","caso",rep(1:nObjetos,nAgentes))
  write.table(t(columnas), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(t(columnas2), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  #X1 <- rdirichlet(nRep1, alfaVec)
  for(i in nRep1i:nRep1f){
    set.seed(500+i)
    X1 <- rdirichlet(1, alfaVec)
    # M=matrix(,nObjetos,nAgentes)
    for(j in 1:nRep2){
      set.seed(1000+nRep2*i+j)
      M <- t(rdirichlet(nAgentes,lambda*(as.vector(X1))))
      
      
      reparto_gre_sol=reparto_gre_ale=reparto_gre_glo=vector("list",length=nAgentes)
      reparto_rr_sol=reparto_rrs_sol=reparto_rrs_glo=vector("list",length=nAgentes)
      reparto_mio=reparto_lipton=reparto_lipton_solo=vector("list",length=nAgentes)
      
      start_time=Sys.time()
      reparto_mio[[1]]=1:nObjetos
      for(l in 1:nRep3){
        reparto_aux=repartoBienes(nAgentes,M)$Art
        if(comparacion_leximin_pp(reparto_mio,reparto_aux,M)==2){
          reparto_mio=reparto_aux
        }
      }
      end_time=Sys.time()
      tiempo_mio[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      mio=felicidad2(reparto_mio,M)
      
      # reparto lipton  
      start_time=Sys.time()
      reparto_lipton[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
        if(comparacion_leximin_pp(reparto_lipton,repartido_aux,M)==2){
          reparto_lipton=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_lip[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      lipton=felicidad2(reparto_lipton,M)
      
      
      # reparto lipton solo. No hace mini simulacion para buscar el mejor.  
      start_time=Sys.time()
      reparto_lipton_solo=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
      end_time=Sys.time()
      tiempo_lip_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      lipton_solo=felicidad2(reparto_lipton_solo,M)
      
      #greedy solo
      
      start_time=Sys.time()
      if(nAgentes==2){
        reparto_gre_sol=greedy_alg_para2(M)  
      }else{
        reparto_gre_sol=greedy_alg(M,1:nAgentes)$reciben
      }
      end_time=Sys.time()
      tiempo_greedy_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_solo=felicidad2(reparto_gre_sol,M)
      
      #robin solo
      start_time=Sys.time()
      reparto_rr_sol=Round_Robin(1:nAgentes,vector("list",nAgentes),1:nObjetos,1:nAgentes,nObjetos,M)$partial_alloc
      
      end_time=Sys.time()
      tiempo_robin_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_solo=felicidad2(reparto_rr_sol,M)
      
      #robin salteador solo
      start_time=Sys.time()
      reparto_rrs_sol=Round_Robin_salteador(1:nAgentes,vector("list",nAgentes),1:nObjetos,1:nAgentes,nObjetos,M)$partial_alloc
      end_time=Sys.time()
      tiempo_robin_salt_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_salt_solo=felicidad2(reparto_rrs_sol,M)
      
      #greedy global
      start_time=Sys.time()
      reparto_gre_glo=greedy_alg_maximiza_leximin(M)
      end_time=Sys.time()
      tiempo_greedy_glob[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_glo=felicidad2(reparto_gre_glo,M)
      
      
      #Robin salteador global
      start_time=Sys.time()
      reparto_rrs_glo=Round_Robin_salteador_maximiza_leximin(M)
      end_time=Sys.time()
      tiempo_robin_glob[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_glo=felicidad2(reparto_rrs_glo,M)
    
      #greedy aleatorio
      start_time=Sys.time()
      reparto_gre_ale[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=greedy_alg_rand(M)$reciben
        if(comparacion_leximin_pp(reparto_gre_ale,repartido_aux,M)==2){
          reparto_gre_ale=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_greedy_alea[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_ale=felicidad2(reparto_gre_ale,M)
  
      
      #alfa_ef
      alfa_ef_mio[i]=mio$alfa_ef
      alfa_ef_lipton_solo[i]=lipton_solo$alfa_ef
      alfa_ef_lipton[i]=lipton$alfa_ef
      alfa_ef_greedy_solo[i]=greedy_solo$alfa_ef
      alfa_ef_robin_solo[i]=robin_solo$alfa_ef
      alfa_ef_robin_salt_solo[i]=robin_salt_solo$alfa_ef
      alfa_ef_greedy_glob[i]=greedy_glo$alfa_ef
      alfa_ef_robin_glob[i]=robin_glo$alfa_ef
      alfa_ef_greedy_alea[i]=greedy_ale$alfa_ef
      
      #alfa_ef1
      alfa_ef1_mio[i]=mio$alfa_ef1[[1]]
      alfa_ef1_lipton_solo[i]=lipton_solo$alfa_ef1[[1]]
      alfa_ef1_lipton[i]=lipton$alfa_ef1[[1]]
      alfa_ef1_greedy_solo[i]=greedy_solo$alfa_ef1[[1]]
      alfa_ef1_robin_solo[i]=robin_solo$alfa_ef1[[1]]
      alfa_ef1_robin_salt_solo[i]=robin_salt_solo$alfa_ef1[[1]]
      alfa_ef1_greedy_glob[i]=greedy_glo$alfa_ef1[[1]]
      alfa_ef1_robin_glob[i]=robin_glo$alfa_ef1[[1]]
      alfa_ef1_greedy_alea[i]=greedy_ale$alfa_ef1[[1]]
      
      #alfa_efx
      alfa_efx_mio[i]=mio$alfa_efx[[1]]
      alfa_efx_lipton_solo[i]=lipton_solo$alfa_efx[[1]]
      alfa_efx_lipton[i]=lipton$alfa_efx[[1]]
      alfa_efx_greedy_solo[i]=greedy_solo$alfa_efx[[1]]
      alfa_efx_robin_solo[i]=robin_solo$alfa_efx[[1]]
      alfa_efx_robin_salt_solo[i]=robin_salt_solo$alfa_efx[[1]]
      alfa_efx_greedy_glob[i]=greedy_glo$alfa_efx[[1]]
      alfa_efx_robin_glob[i]=robin_glo$alfa_efx[[1]]
      alfa_efx_greedy_alea[i]=greedy_ale$alfa_efx[[1]]
      
      
      #alfa_prop
      alfa_prop_mio[i]=mio$alfa_prop
      alfa_prop_lipton_solo[i]=lipton_solo$alfa_prop
      alfa_prop_lipton[i]=lipton$alfa_prop
      alfa_prop_greedy_solo[i]=greedy_solo$alfa_prop
      alfa_prop_robin_solo[i]=robin_solo$alfa_prop
      alfa_prop_robin_salt_solo[i]=robin_salt_solo$alfa_prop
      alfa_prop_greedy_glob[i]=greedy_glo$alfa_prop
      alfa_prop_robin_glob[i]=robin_glo$alfa_prop
      alfa_prop_greedy_alea[i]=greedy_ale$alfa_prop
      
      #alfa_prop1
      alfa_prop1_mio[i]=mio$alfa_prop1[[1]]
      alfa_prop1_lipton_solo[i]=lipton_solo$alfa_prop1[[1]]
      alfa_prop1_lipton[i]=lipton$alfa_prop1[[1]]
      alfa_prop1_greedy_solo[i]=greedy_solo$alfa_prop1[[1]]
      alfa_prop1_robin_solo[i]=robin_solo$alfa_prop1[[1]]
      alfa_prop1_robin_salt_solo[i]=robin_salt_solo$alfa_prop1[[1]]
      alfa_prop1_greedy_glob[i]=greedy_glo$alfa_prop1[[1]]
      alfa_prop1_robin_glob[i]=robin_glo$alfa_prop1[[1]]
      alfa_prop1_greedy_alea[i]=greedy_ale$alfa_prop1[[1]]
      
      #alfa_propx
      alfa_propx_mio[i]=mio$alfa_propx[[1]]
      alfa_propx_lipton_solo[i]=lipton_solo$alfa_propx[[1]]
      alfa_propx_lipton[i]=lipton$alfa_propx[[1]]
      alfa_propx_greedy_solo[i]=greedy_solo$alfa_propx[[1]]
      alfa_propx_robin_solo[i]=robin_solo$alfa_propx[[1]]
      alfa_propx_robin_salt_solo[i]=robin_salt_solo$alfa_propx[[1]]
      alfa_propx_greedy_glob[i]=greedy_glo$alfa_propx[[1]]
      alfa_propx_robin_glob[i]=robin_glo$alfa_propx[[1]]
      alfa_propx_greedy_alea[i]=greedy_ale$alfa_propx[[1]]
      
      #ef
      ef_mio[i]=mio$ef
      ef_lipton_solo[i]=lipton_solo$ef
      ef_lipton[i]=lipton$ef
      ef_greedy_solo[i]=greedy_solo$ef
      ef_robin_solo[i]=robin_solo$ef
      ef_robin_salt_solo[i]=robin_salt_solo$ef
      ef_greedy_glob[i]=greedy_glo$ef
      ef_robin_glob[i]=robin_glo$ef
      ef_greedy_alea[i]=greedy_ale$ef
            
      ef1_mio[i]=mio$ef1[[1]]
      ef1_lipton_solo[i]=lipton_solo$ef1[[1]]
      ef1_lipton[i]=lipton$ef1[[1]]
      ef1_greedy_solo[i]=greedy_solo$ef1[[1]]
      ef1_robin_solo[i]=robin_solo$ef1[[1]]
      ef1_robin_salt_solo[i]=robin_salt_solo$ef1[[1]]
      ef1_greedy_glob[i]=greedy_glo$ef1[[1]]
      ef1_robin_glob[i]=robin_glo$ef1[[1]]
      ef1_greedy_alea[i]=greedy_ale$ef1[[1]]
      
      #efx
      efx_mio[i]=mio$efx[[1]]
      efx_lipton_solo[i]=lipton_solo$efx[[1]]
      efx_lipton[i]=lipton$efx[[1]]
      efx_greedy_solo[i]=greedy_solo$efx[[1]]
      efx_robin_solo[i]=robin_solo$efx[[1]]
      efx_robin_salt_solo[i]=robin_salt_solo$efx[[1]]
      efx_greedy_glob[i]=greedy_glo$efx[[1]]
      efx_robin_glob[i]=robin_glo$efx[[1]]
      efx_greedy_alea[i]=greedy_ale$efx[[1]]
      
      #envidia maxima
    
      envidiaMaxima_mio[i]=max(mio$envidiaMaxima)
      envidiaMaxima_lipton_solo[i]=max(lipton_solo$envidiaMaxima)
      envidiaMaxima_lipton[i]=max(lipton$envidiaMaxima)
      envidiaMaxima_greedy_solo[i]=max(greedy_solo$envidiaMaxima)
      envidiaMaxima_robin_solo[i]=max(robin_solo$envidiaMaxima)
      envidiaMaxima_robin_salt_solo[i]=max(robin_salt_solo$envidiaMaxima)
      envidiaMaxima_greedy_glob[i]=max(greedy_glo$envidiaMaxima)
      envidiaMaxima_robin_glob[i]=max(robin_glo$envidiaMaxima)
      envidiaMaxima_greedy_alea[i]=max(greedy_ale$envidiaMaxima)
      
      #envidiosos
      envidiosos_mio[i]=mio$envidiosos
      envidiosos_lipton_solo[i]=lipton_solo$envidiosos
      envidiosos_lipton[i]=lipton$envidiosos
      envidiosos_greedy_solo[i]=greedy_solo$envidiosos
      envidiosos_robin_solo[i]=robin_solo$envidiosos
      envidiosos_robin_salt_solo[i]=robin_salt_solo$envidiosos
      envidiosos_greedy_glob[i]=greedy_glo$envidiosos
      envidiosos_robin_glob[i]=robin_glo$envidiosos
      envidiosos_greedy_alea[i]=greedy_ale$envidiosos
      
      #proporcionalidad
      proporcionalidad_mio[i]=mio$proporcionalidad
      proporcionalidad_lipton_solo[i]=lipton_solo$proporcionalidad
      proporcionalidad_lipton[i]=lipton$proporcionalidad
      proporcionalidad_greedy_solo[i]=greedy_solo$proporcionalidad
      proporcionalidad_robin_solo[i]=robin_solo$proporcionalidad
      proporcionalidad_robin_salt_solo[i]=robin_salt_solo$proporcionalidad
      proporcionalidad_greedy_glob[i]=greedy_glo$proporcionalidad
      proporcionalidad_robin_glob[i]=robin_glo$proporcionalidad
      proporcionalidad_greedy_alea[i]=greedy_ale$proporcionalidad
      
      
      #bien Nash
      bienNash_mio[i]=mio$bienNash
      bienNash_lipton_solo[i]=lipton_solo$bienNash
      bienNash_lipton[i]=lipton$bienNash
      bienNash_greedy_solo[i]=greedy_solo$bienNash
      bienNash_robin_solo[i]=robin_solo$bienNash
      bienNash_robin_salt_solo[i]=robin_salt_solo$bienNash
      bienNash_greedy_glob[i]=greedy_glo$bienNash
      bienNash_robin_glob[i]=robin_glo$bienNash
      bienNash_greedy_alea[i]=greedy_ale$bienNash
      
      #bien social
      bienSocial_mio[i]=mio$bienSocial
      bienSocial_lipton_solo[i]=lipton_solo$bienSocial
      bienSocial_lipton[i]=lipton$bienSocial
      bienSocial_greedy_solo[i]=greedy_solo$bienSocial
      bienSocial_robin_solo[i]=robin_solo$bienSocial
      bienSocial_robin_salt_solo[i]=robin_salt_solo$bienSocial
      bienSocial_greedy_glob[i]=greedy_glo$bienSocial
      bienSocial_robin_glob[i]=robin_glo$bienSocial
      bienSocial_greedy_alea[i]=greedy_ale$bienSocial
      
      
      #menos lleva
      menosLleva_mio[i]=mio$menosLleva
      menosLleva_lipton_solo[i]=lipton_solo$menosLleva
      menosLleva_lipton[i]=lipton$menosLleva
      menosLleva_greedy_solo[i]=greedy_solo$menosLleva
      menosLleva_robin_solo[i]=robin_solo$menosLleva
      menosLleva_robin_salt_solo[i]=robin_salt_solo$menosLleva
      menosLleva_greedy_glob[i]=greedy_glo$menosLleva
      menosLleva_robin_glob[i]=robin_glo$menosLleva
      menosLleva_greedy_alea[i]=greedy_ale$menosLleva
      
      
      
      esProporcionalEst[i]=max(mio$proporcionalidad,lipton_solo$proporcionalidad,lipton$proporcionalidad,greedy_solo$proporcionalidad,robin_solo$proporcionalidad,robin_salt_solo$proporcionalidad,greedy_glo$proporcionalidad,robin_glo$proporcionalidad,greedy_ale$proporcionalidad)
      
      alfa_efs=c(alfa_ef_mio[i],alfa_ef_lipton_solo[i],alfa_ef_lipton[i],alfa_ef_greedy_solo[i],alfa_ef_robin_solo[i],alfa_ef_robin_salt_solo[i],alfa_ef_greedy_glob[i],alfa_ef_robin_glob[i],alfa_ef_greedy_alea[i])
      alfa_ef1s=c(alfa_ef1_mio[i],alfa_ef1_lipton_solo[i],alfa_ef1_lipton[i],alfa_ef1_greedy_solo[i],alfa_ef1_robin_solo[i],alfa_ef1_robin_salt_solo[i],alfa_ef1_greedy_glob[i],alfa_ef1_robin_glob[i],alfa_ef1_greedy_alea[i])
      alfa_efxs=c(alfa_efx_mio[i],alfa_efx_lipton_solo[i],alfa_efx_lipton[i],alfa_efx_greedy_solo[i],alfa_efx_robin_solo[i],alfa_efx_robin_salt_solo[i],alfa_efx_greedy_glob[i],alfa_efx_robin_glob[i],alfa_efx_greedy_alea[i])
      alfa_props=c(alfa_prop_mio[i],alfa_prop_lipton_solo[i],alfa_prop_lipton[i],alfa_prop_greedy_solo[i],alfa_prop_robin_solo[i],alfa_prop_robin_salt_solo[i],alfa_prop_greedy_glob[i],alfa_prop_robin_glob[i],alfa_prop_greedy_alea[i])
      alfa_prop1s=c(alfa_prop1_mio[i],alfa_prop1_lipton_solo[i],alfa_prop1_lipton[i],alfa_prop1_greedy_solo[i],alfa_prop1_robin_solo[i],alfa_prop1_robin_salt_solo[i],alfa_prop1_greedy_glob[i],alfa_prop1_robin_glob[i],alfa_prop1_greedy_alea[i])
      alfa_propxs=c(alfa_propx_mio[i],alfa_propx_lipton_solo[i],alfa_propx_lipton[i],alfa_propx_greedy_solo[i],alfa_propx_robin_solo[i],alfa_propx_robin_salt_solo[i],alfa_propx_greedy_glob[i],alfa_propx_robin_glob[i],alfa_propx_greedy_alea[i])
      efs=c(ef_mio[i],ef_lipton_solo[i],ef_lipton[i],ef_greedy_solo[i],ef_robin_solo[i],ef_robin_salt_solo[i],ef_greedy_glob[i],ef_robin_glob[i],ef_greedy_alea[i])
      ef1s=c(ef1_mio[i],ef1_lipton_solo[i],ef1_lipton[i],ef1_greedy_solo[i],ef1_robin_solo[i],ef1_robin_salt_solo[i],ef1_greedy_glob[i],ef1_robin_glob[i],ef1_greedy_alea[i])
      efxs=c(efx_mio[i],efx_lipton_solo[i],efx_lipton[i],efx_greedy_solo[i],efx_robin_solo[i],efx_robin_salt_solo[i],efx_greedy_glob[i],efx_robin_glob[i],efx_greedy_alea[i])
      enviMaxs=c(envidiaMaxima_mio[i],envidiaMaxima_lipton_solo[i],envidiaMaxima_lipton[i],envidiaMaxima_greedy_solo[i],envidiaMaxima_robin_solo[i],envidiaMaxima_robin_salt_solo[i],envidiaMaxima_greedy_glob[i],envidiaMaxima_robin_glob[i],envidiaMaxima_greedy_alea[i])
      envis=c(envidiosos_mio[i],envidiosos_lipton_solo[i],envidiosos_lipton[i],envidiosos_greedy_solo[i],envidiosos_robin_solo[i],envidiosos_robin_salt_solo[i],envidiosos_greedy_glob[i],envidiosos_robin_glob[i],envidiosos_greedy_alea[i])
      props=c(proporcionalidad_mio[i],proporcionalidad_lipton_solo[i],proporcionalidad_lipton[i],proporcionalidad_greedy_solo[i],proporcionalidad_robin_solo[i],proporcionalidad_robin_salt_solo[i],proporcionalidad_greedy_glob[i],proporcionalidad_robin_glob[i],proporcionalidad_greedy_alea[i])
      nashs=c(bienNash_mio[i],bienNash_lipton_solo[i],bienNash_lipton[i],bienNash_greedy_solo[i],bienNash_robin_solo[i],bienNash_robin_salt_solo[i],bienNash_greedy_glob[i],bienNash_robin_glob[i],bienNash_greedy_alea[i])
      socials=c(bienSocial_mio[i],bienSocial_lipton_solo[i],bienSocial_lipton[i],bienSocial_greedy_solo[i],bienSocial_robin_solo[i],bienSocial_robin_salt_solo[i],bienSocial_greedy_glob[i],bienSocial_robin_glob[i],bienSocial_greedy_alea[i])
      menosLlevan=c(menosLleva_mio[i],menosLleva_lipton_solo[i],menosLleva_lipton[i],menosLleva_greedy_solo[i],menosLleva_robin_solo[i],menosLleva_robin_salt_solo[i],menosLleva_greedy_glob[i],menosLleva_robin_glob[i],menosLleva_greedy_alea[i])
      tiempos=c(tiempo_mio[i],tiempo_lip_solo[i],tiempo_lip[i],tiempo_greedy_solo[i],tiempo_robin_solo[i],tiempo_robin_salt_solo[i],tiempo_greedy_glob[i],tiempo_robin_glob[i],tiempo_greedy_alea[i])
      
      guardo=c(i,j,alfa_efs,alfa_ef1s,alfa_efxs,alfa_props,alfa_prop1s,alfa_propxs,efs,ef1s,efxs,enviMaxs,envis,props,nashs,socials,menosLlevan,esProporcionalEst[i],tiempos)
      guardo_inst=c(i,j,as.vector(M))
      write.table(t(guardo), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(t(guardo_inst), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      print(paste(nAgentes,"_agentes_",nObjetos,"_objetos_","lambda_",lambda,"_repeticion_", i, "_caso_", j,sep=""))
    }
  }  
}

#la siguiente funcion es para la simulacion de los 
#algoritmos no exhaustivos en los casos exhaustivos.
# cambia la matriz de valoraciones
simulacionSinExh_exh = function(nRep1i,nRep1f,nRep2,nRep3,nObjetos,nAgentes,lambda){
  #nRep1=10
  alfa_ef_mio=alfa_ef_lipton=alfa_ef_lipton_solo=alfa_ef_greedy_solo=alfa_ef_robin_solo=alfa_ef_robin_salt_solo=alfa_ef_greedy_glob=alfa_ef_robin_glob=alfa_ef_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_ef1_mio=alfa_ef1_lipton=alfa_ef1_lipton_solo=alfa_ef1_greedy_solo=alfa_ef1_robin_solo=alfa_ef1_robin_salt_solo=alfa_ef1_greedy_glob=alfa_ef1_robin_glob=alfa_ef1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_efx_mio=alfa_efx_lipton=alfa_efx_lipton_solo=alfa_efx_greedy_solo=alfa_efx_robin_solo=alfa_efx_robin_salt_solo=alfa_efx_greedy_glob=alfa_efx_robin_glob=alfa_efx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_prop_mio=alfa_prop_lipton=alfa_prop_lipton_solo=alfa_prop_greedy_solo=alfa_prop_robin_solo=alfa_prop_robin_salt_solo=alfa_prop_greedy_glob=alfa_prop_robin_glob=alfa_prop_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_prop1_mio=alfa_prop1_lipton=alfa_prop1_lipton_solo=alfa_prop1_greedy_solo=alfa_prop1_robin_solo=alfa_prop1_robin_salt_solo=alfa_prop1_greedy_glob=alfa_prop1_robin_glob=alfa_prop1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  alfa_propx_mio=alfa_propx_lipton=alfa_propx_lipton_solo=alfa_propx_greedy_solo=alfa_propx_robin_solo=alfa_propx_robin_salt_solo=alfa_propx_greedy_glob=alfa_propx_robin_glob=alfa_propx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  ef_mio=ef_lipton=ef_lipton_solo=ef_greedy_solo=ef_robin_solo=ef_robin_salt_solo=ef_greedy_glob=ef_robin_glob=ef_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  ef1_mio=ef1_lipton=ef1_lipton_solo=ef1_greedy_solo=ef1_robin_solo=ef1_robin_salt_solo=ef1_greedy_glob=ef1_robin_glob=ef1_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  efx_mio=efx_lipton=efx_lipton_solo=efx_greedy_solo=efx_robin_solo=efx_robin_salt_solo=efx_greedy_glob=efx_robin_glob=efx_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  envidiaMaxima_mio=envidiaMaxima_lipton=envidiaMaxima_lipton_solo=envidiaMaxima_greedy_solo=envidiaMaxima_robin_solo=envidiaMaxima_robin_salt_solo=envidiaMaxima_greedy_glob=envidiaMaxima_robin_glob=envidiaMaxima_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  envidiosos_mio=envidiosos_lipton=envidiosos_lipton_solo=envidiosos_greedy_solo=envidiosos_robin_solo=envidiosos_robin_salt_solo=envidiosos_greedy_glob=envidiosos_robin_glob=envidiosos_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  proporcionalidad_mio=proporcionalidad_lipton=proporcionalidad_lipton_solo=proporcionalidad_greedy_solo=proporcionalidad_robin_solo=proporcionalidad_robin_salt_solo=proporcionalidad_greedy_glob=proporcionalidad_robin_glob=proporcionalidad_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  bienSocial_mio=bienSocial_lipton=bienSocial_lipton_solo=bienSocial_greedy_solo=bienSocial_robin_solo=bienSocial_robin_salt_solo=bienSocial_greedy_glob=bienSocial_robin_glob=bienSocial_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  bienNash_mio=bienNash_lipton=bienNash_lipton_solo=bienNash_greedy_solo=bienNash_robin_solo=bienNash_robin_salt_solo=bienNash_greedy_glob=bienNash_robin_glob=bienNash_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  menosLleva_mio=menosLleva_lipton=menosLleva_lipton_solo=menosLleva_greedy_solo=menosLleva_robin_solo=menosLleva_robin_salt_solo=menosLleva_greedy_glob=menosLleva_robin_glob=menosLleva_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  esProporcionalEst=vector(,length=(nRep1f-nRep1i+1)) #ahora es una estimación, si encuentra algún reparto proporcional da 1
  tiempo_mio=tiempo_lip=tiempo_lip_solo=tiempo_greedy_solo=tiempo_robin_solo=tiempo_robin_salt_solo=tiempo_greedy_glob=tiempo_robin_glob=tiempo_greedy_alea=vector(,length=(nRep1f-nRep1i+1))
  
  valores_c_simul=read.table("valores_c_Dirichlet2.txt",header=TRUE)
  caso=intersect(which(round(valores_c_simul$E,3)==round(1/nAgentes,3)),which(valores_c_simul$k==nObjetos))
  valor_c=valores_c_simul$c[caso]
  alfaVec=rep(valor_c,nObjetos)
  #alfaVec=rep(1,nObjetos)
  nombre_archi=paste("reparto_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  nombre_archi_inst=paste("instancia_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  archivo_resultados <- nombre_archi
  archivo_instancia <- nombre_archi_inst
  
  alfa_ef_nombres=c("alfa_ef_mio","alfa_ef_lip_sol","alfa_ef_lip","alfa_ef_gre_sol","alfa_ef_rob_sol","alfa_ef_rob_salt_sol","alfa_ef_gre_glo","alfa_ef_rob_glo","alfa_ef_gre_ale")
  alfa_ef1_nombres=c("alfa_ef1_mio","alfa_ef1_lip_sol","alfa_ef1_lip","alfa_ef1_gre_sol","alfa_ef1_rob_sol","alfa_ef1_rob_salt_sol","alfa_ef1_gre_glo","alfa_ef1_rob_glo","alfa_ef1_gre_ale")
  alfa_efx_nombres=c("alfa_efx_mio","alfa_efx_lip_sol","alfa_efx_lip","alfa_efx_gre_sol","alfa_efx_rob_sol","alfa_efx_rob_salt_sol","alfa_efx_gre_glo","alfa_efx_rob_glo","alfa_efx_gre_ale")
  alfa_prop_nombres=c("alfa_prop_mio","alfa_prop_lip_sol","alfa_prop_lip","alfa_prop_gre_sol","alfa_prop_rob_sol","alfa_prop_rob_salt_sol","alfa_prop_gre_glo","alfa_prop_rob_glo","alfa_prop_gre_ale")
  alfa_prop1_nombres=c("alfa_prop1_mio","alfa_prop1_lip_sol","alfa_prop1_lip","alfa_prop1_gre_sol","alfa_prop1_rob_sol","alfa_prop1_rob_salt_sol","alfa_prop1_gre_glo","alfa_prop1_rob_glo","alfa_prop1_gre_ale")
  alfa_propx_nombres=c("alfa_propx_mio","alfa_propx_lip_sol","alfa_propx_lip","alfa_propx_gre_sol","alfa_propx_rob_sol","alfa_propx_rob_salt_sol","alfa_propx_gre_glo","alfa_propx_rob_glo","alfa_propx_gre_ale")
  ef_nombres=c("ef_mio","ef_lip_sol","ef_lip","ef_gre_sol","ef_rob_sol","ef_rob_salt_sol","ef_gre_glo","ef_rob_glo","ef_gre_ale")
  ef1_nombres=c("ef1_mio","ef1_lip_sol","ef1_lip","ef1_gre_sol","ef1_rob_sol","ef1_rob_salt_sol","ef1_gre_glo","ef1_rob_glo","ef1_gre_ale")
  efx_nombres=c("efx_mio","efx_lip_sol","efx_lip","efx_gre_sol","efx_rob_sol","efx_rob_salt_sol","efx_gre_glo","efx_rob_glo","efx_gre_ale")
  envyMax_nombres=c("envyMax_mio","envyMax_lip_sol","envyMax_lip","envyMax_gre_sol","envyMax_rob_sol","envyMax_rob_salt_sol","envyMax_gre_glo","envyMax_rob_glo","envyMax_gre_ale")
  envis_nombres=c("envis_mio","envis_lip_sol","envis_lip","envis_gre_sol","envis_rob_sol","envis_rob_salt_sol","envis_gre_glo","envis_rob_glo","envis_gre_ale")
  prop_nombres=c("prop_mio","prop_lip_sol","prop_lip","prop_gre_sol","prop_rob_sol","prop_rob_salt_sol","prop_gre_glo","prop_rob_glo","prop_gre_ale")
  nash_nombres=c("nash_mio","nash_lip_sol","nash_lip","nash_gre_sol","nash_rob_sol","nash_rob_salt_sol","nash_gre_glo","nash_rob_glo","nash_gre_ale")
  social_nombres=c("social_mio","social_lip_sol","social_lip","social_gre_sol","social_rob_sol","social_rob_salt_sol","social_gre_glo","social_rob_glo","social_gre_ale")
  menosLleva_nombres=c("menosLleva_mio","menosLleva_lip_sol","menosLleva_lip","menosLleva_gre_sol","menosLleva_rob_sol","menosLleva_rob_salt_sol","menosLleva_gre_glo","menosLleva_rob_glo","menosLleva_gre_ale")
  esProporcional_nombres=c("proporcional")
  tiempo_nombres=c("tiempo_mio","tiempo_lip_sol","tiempo_lip","tiempo_gre_sol","tiempo_rob_sol","tiempo_rob_salt_sol","tiempo_gre_glo","tiempo_rob_glo","tiempo_gre_ale")
  columnas=c("rep","caso",alfa_ef_nombres,alfa_ef1_nombres,alfa_efx_nombres,alfa_prop_nombres,alfa_prop1_nombres,alfa_propx_nombres,ef_nombres,ef1_nombres,efx_nombres,envyMax_nombres,envis_nombres,prop_nombres,nash_nombres,social_nombres,menosLleva_nombres,esProporcional_nombres,tiempo_nombres)
  columnas2=c("rep","caso",rep(1:nObjetos,nAgentes))
  write.table(t(columnas), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(t(columnas2), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  #X1 <- rdirichlet(nRep1, alfaVec)
  for(i in nRep1i:nRep1f){
    set.seed(500+i)
    X1 <- rdirichlet(1, alfaVec)
    # M=matrix(,nObjetos,nAgentes)
    for(j in 1:nRep2){
      set.seed(1000+nRep2*i+j)
      M <- t(rdirichlet(nAgentes,lambda*(as.vector(X1))))
      
      
      reparto_gre_sol=reparto_gre_ale=reparto_gre_glo=vector("list",length=nAgentes)
      reparto_rr_sol=reparto_rrs_sol=reparto_rrs_glo=vector("list",length=nAgentes)
      reparto_mio=reparto_lipton=reparto_lipton_solo=vector("list",length=nAgentes)
      
      start_time=Sys.time()
      reparto_mio[[1]]=1:nObjetos
      for(l in 1:nRep3){
        reparto_aux=repartoBienes(nAgentes,M)$Art
        if(comparacion_leximin_pp(reparto_mio,reparto_aux,M)==2){
          reparto_mio=reparto_aux
        }
      }
      end_time=Sys.time()
      tiempo_mio[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      mio=felicidad2(reparto_mio,M)
      
      # reparto lipton  
      start_time=Sys.time()
      reparto_lipton[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
        if(comparacion_leximin_pp(reparto_lipton,repartido_aux,M)==2){
          reparto_lipton=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_lip[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      lipton=felicidad2(reparto_lipton,M)
      
      
      # reparto lipton solo. No hace mini simulacion para buscar el mejor.  
      start_time=Sys.time()
      reparto_lipton_solo=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
      end_time=Sys.time()
      tiempo_lip_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      lipton_solo=felicidad2(reparto_lipton_solo,M)
      
      #greedy solo
      
      start_time=Sys.time()
      if(nAgentes==2){
        reparto_gre_sol=greedy_alg_para2(M)  
      }else{
        reparto_gre_sol=greedy_alg(M,1:nAgentes)$reciben
      }
      end_time=Sys.time()
      tiempo_greedy_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_solo=felicidad2(reparto_gre_sol,M)
      
      #robin solo
      start_time=Sys.time()
      reparto_rr_sol=Round_Robin(1:nAgentes,vector("list",nAgentes),1:nObjetos,1:nAgentes,nObjetos,M)$partial_alloc
      
      end_time=Sys.time()
      tiempo_robin_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_solo=felicidad2(reparto_rr_sol,M)
      
      #robin salteador solo
      start_time=Sys.time()
      reparto_rrs_sol=Round_Robin_salteador(1:nAgentes,vector("list",nAgentes),1:nObjetos,1:nAgentes,nObjetos,M)$partial_alloc
      end_time=Sys.time()
      tiempo_robin_salt_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_salt_solo=felicidad2(reparto_rrs_sol,M)
      
      #greedy global
      start_time=Sys.time()
      reparto_gre_glo=greedy_alg_maximiza_leximin(M)
      end_time=Sys.time()
      tiempo_greedy_glob[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_glo=felicidad2(reparto_gre_glo,M)
      
      
      #Robin salteador global
      start_time=Sys.time()
      reparto_rrs_glo=Round_Robin_salteador_maximiza_leximin(M)
      end_time=Sys.time()
      tiempo_robin_glob[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin_glo=felicidad2(reparto_rrs_glo,M)
      
      #greedy aleatorio
      start_time=Sys.time()
      reparto_gre_ale[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=greedy_alg_rand(M)$reciben
        if(comparacion_leximin_pp(reparto_gre_ale,repartido_aux,M)==2){
          reparto_gre_ale=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_greedy_alea[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      greedy_ale=felicidad2(reparto_gre_ale,M)
      
      
      #alfa_ef
      alfa_ef_mio[i]=mio$alfa_ef
      alfa_ef_lipton_solo[i]=lipton_solo$alfa_ef
      alfa_ef_lipton[i]=lipton$alfa_ef
      alfa_ef_greedy_solo[i]=greedy_solo$alfa_ef
      alfa_ef_robin_solo[i]=robin_solo$alfa_ef
      alfa_ef_robin_salt_solo[i]=robin_salt_solo$alfa_ef
      alfa_ef_greedy_glob[i]=greedy_glo$alfa_ef
      alfa_ef_robin_glob[i]=robin_glo$alfa_ef
      alfa_ef_greedy_alea[i]=greedy_ale$alfa_ef
      
      #alfa_ef1
      alfa_ef1_mio[i]=mio$alfa_ef1[[1]]
      alfa_ef1_lipton_solo[i]=lipton_solo$alfa_ef1[[1]]
      alfa_ef1_lipton[i]=lipton$alfa_ef1[[1]]
      alfa_ef1_greedy_solo[i]=greedy_solo$alfa_ef1[[1]]
      alfa_ef1_robin_solo[i]=robin_solo$alfa_ef1[[1]]
      alfa_ef1_robin_salt_solo[i]=robin_salt_solo$alfa_ef1[[1]]
      alfa_ef1_greedy_glob[i]=greedy_glo$alfa_ef1[[1]]
      alfa_ef1_robin_glob[i]=robin_glo$alfa_ef1[[1]]
      alfa_ef1_greedy_alea[i]=greedy_ale$alfa_ef1[[1]]
      
      #alfa_efx
      alfa_efx_mio[i]=mio$alfa_efx[[1]]
      alfa_efx_lipton_solo[i]=lipton_solo$alfa_efx[[1]]
      alfa_efx_lipton[i]=lipton$alfa_efx[[1]]
      alfa_efx_greedy_solo[i]=greedy_solo$alfa_efx[[1]]
      alfa_efx_robin_solo[i]=robin_solo$alfa_efx[[1]]
      alfa_efx_robin_salt_solo[i]=robin_salt_solo$alfa_efx[[1]]
      alfa_efx_greedy_glob[i]=greedy_glo$alfa_efx[[1]]
      alfa_efx_robin_glob[i]=robin_glo$alfa_efx[[1]]
      alfa_efx_greedy_alea[i]=greedy_ale$alfa_efx[[1]]
      
      
      #alfa_prop
      alfa_prop_mio[i]=mio$alfa_prop
      alfa_prop_lipton_solo[i]=lipton_solo$alfa_prop
      alfa_prop_lipton[i]=lipton$alfa_prop
      alfa_prop_greedy_solo[i]=greedy_solo$alfa_prop
      alfa_prop_robin_solo[i]=robin_solo$alfa_prop
      alfa_prop_robin_salt_solo[i]=robin_salt_solo$alfa_prop
      alfa_prop_greedy_glob[i]=greedy_glo$alfa_prop
      alfa_prop_robin_glob[i]=robin_glo$alfa_prop
      alfa_prop_greedy_alea[i]=greedy_ale$alfa_prop
      
      #alfa_prop1
      alfa_prop1_mio[i]=mio$alfa_prop1[[1]]
      alfa_prop1_lipton_solo[i]=lipton_solo$alfa_prop1[[1]]
      alfa_prop1_lipton[i]=lipton$alfa_prop1[[1]]
      alfa_prop1_greedy_solo[i]=greedy_solo$alfa_prop1[[1]]
      alfa_prop1_robin_solo[i]=robin_solo$alfa_prop1[[1]]
      alfa_prop1_robin_salt_solo[i]=robin_salt_solo$alfa_prop1[[1]]
      alfa_prop1_greedy_glob[i]=greedy_glo$alfa_prop1[[1]]
      alfa_prop1_robin_glob[i]=robin_glo$alfa_prop1[[1]]
      alfa_prop1_greedy_alea[i]=greedy_ale$alfa_prop1[[1]]
      
      #alfa_propx
      alfa_propx_mio[i]=mio$alfa_propx[[1]]
      alfa_propx_lipton_solo[i]=lipton_solo$alfa_propx[[1]]
      alfa_propx_lipton[i]=lipton$alfa_propx[[1]]
      alfa_propx_greedy_solo[i]=greedy_solo$alfa_propx[[1]]
      alfa_propx_robin_solo[i]=robin_solo$alfa_propx[[1]]
      alfa_propx_robin_salt_solo[i]=robin_salt_solo$alfa_propx[[1]]
      alfa_propx_greedy_glob[i]=greedy_glo$alfa_propx[[1]]
      alfa_propx_robin_glob[i]=robin_glo$alfa_propx[[1]]
      alfa_propx_greedy_alea[i]=greedy_ale$alfa_propx[[1]]
      
      #ef
      ef_mio[i]=mio$ef
      ef_lipton_solo[i]=lipton_solo$ef
      ef_lipton[i]=lipton$ef
      ef_greedy_solo[i]=greedy_solo$ef
      ef_robin_solo[i]=robin_solo$ef
      ef_robin_salt_solo[i]=robin_salt_solo$ef
      ef_greedy_glob[i]=greedy_glo$ef
      ef_robin_glob[i]=robin_glo$ef
      ef_greedy_alea[i]=greedy_ale$ef
      
      ef1_mio[i]=mio$ef1[[1]]
      ef1_lipton_solo[i]=lipton_solo$ef1[[1]]
      ef1_lipton[i]=lipton$ef1[[1]]
      ef1_greedy_solo[i]=greedy_solo$ef1[[1]]
      ef1_robin_solo[i]=robin_solo$ef1[[1]]
      ef1_robin_salt_solo[i]=robin_salt_solo$ef1[[1]]
      ef1_greedy_glob[i]=greedy_glo$ef1[[1]]
      ef1_robin_glob[i]=robin_glo$ef1[[1]]
      ef1_greedy_alea[i]=greedy_ale$ef1[[1]]
      
      #efx
      efx_mio[i]=mio$efx[[1]]
      efx_lipton_solo[i]=lipton_solo$efx[[1]]
      efx_lipton[i]=lipton$efx[[1]]
      efx_greedy_solo[i]=greedy_solo$efx[[1]]
      efx_robin_solo[i]=robin_solo$efx[[1]]
      efx_robin_salt_solo[i]=robin_salt_solo$efx[[1]]
      efx_greedy_glob[i]=greedy_glo$efx[[1]]
      efx_robin_glob[i]=robin_glo$efx[[1]]
      efx_greedy_alea[i]=greedy_ale$efx[[1]]
      
      #envidia maxima
      
      envidiaMaxima_mio[i]=max(mio$envidiaMaxima)
      envidiaMaxima_lipton_solo[i]=max(lipton_solo$envidiaMaxima)
      envidiaMaxima_lipton[i]=max(lipton$envidiaMaxima)
      envidiaMaxima_greedy_solo[i]=max(greedy_solo$envidiaMaxima)
      envidiaMaxima_robin_solo[i]=max(robin_solo$envidiaMaxima)
      envidiaMaxima_robin_salt_solo[i]=max(robin_salt_solo$envidiaMaxima)
      envidiaMaxima_greedy_glob[i]=max(greedy_glo$envidiaMaxima)
      envidiaMaxima_robin_glob[i]=max(robin_glo$envidiaMaxima)
      envidiaMaxima_greedy_alea[i]=max(greedy_ale$envidiaMaxima)
      
      #envidiosos
      envidiosos_mio[i]=mio$envidiosos
      envidiosos_lipton_solo[i]=lipton_solo$envidiosos
      envidiosos_lipton[i]=lipton$envidiosos
      envidiosos_greedy_solo[i]=greedy_solo$envidiosos
      envidiosos_robin_solo[i]=robin_solo$envidiosos
      envidiosos_robin_salt_solo[i]=robin_salt_solo$envidiosos
      envidiosos_greedy_glob[i]=greedy_glo$envidiosos
      envidiosos_robin_glob[i]=robin_glo$envidiosos
      envidiosos_greedy_alea[i]=greedy_ale$envidiosos
      
      #proporcionalidad
      proporcionalidad_mio[i]=mio$proporcionalidad
      proporcionalidad_lipton_solo[i]=lipton_solo$proporcionalidad
      proporcionalidad_lipton[i]=lipton$proporcionalidad
      proporcionalidad_greedy_solo[i]=greedy_solo$proporcionalidad
      proporcionalidad_robin_solo[i]=robin_solo$proporcionalidad
      proporcionalidad_robin_salt_solo[i]=robin_salt_solo$proporcionalidad
      proporcionalidad_greedy_glob[i]=greedy_glo$proporcionalidad
      proporcionalidad_robin_glob[i]=robin_glo$proporcionalidad
      proporcionalidad_greedy_alea[i]=greedy_ale$proporcionalidad
      
      
      #bien Nash
      bienNash_mio[i]=mio$bienNash
      bienNash_lipton_solo[i]=lipton_solo$bienNash
      bienNash_lipton[i]=lipton$bienNash
      bienNash_greedy_solo[i]=greedy_solo$bienNash
      bienNash_robin_solo[i]=robin_solo$bienNash
      bienNash_robin_salt_solo[i]=robin_salt_solo$bienNash
      bienNash_greedy_glob[i]=greedy_glo$bienNash
      bienNash_robin_glob[i]=robin_glo$bienNash
      bienNash_greedy_alea[i]=greedy_ale$bienNash
      
      #bien social
      bienSocial_mio[i]=mio$bienSocial
      bienSocial_lipton_solo[i]=lipton_solo$bienSocial
      bienSocial_lipton[i]=lipton$bienSocial
      bienSocial_greedy_solo[i]=greedy_solo$bienSocial
      bienSocial_robin_solo[i]=robin_solo$bienSocial
      bienSocial_robin_salt_solo[i]=robin_salt_solo$bienSocial
      bienSocial_greedy_glob[i]=greedy_glo$bienSocial
      bienSocial_robin_glob[i]=robin_glo$bienSocial
      bienSocial_greedy_alea[i]=greedy_ale$bienSocial
      
      
      #menos lleva
      menosLleva_mio[i]=mio$menosLleva
      menosLleva_lipton_solo[i]=lipton_solo$menosLleva
      menosLleva_lipton[i]=lipton$menosLleva
      menosLleva_greedy_solo[i]=greedy_solo$menosLleva
      menosLleva_robin_solo[i]=robin_solo$menosLleva
      menosLleva_robin_salt_solo[i]=robin_salt_solo$menosLleva
      menosLleva_greedy_glob[i]=greedy_glo$menosLleva
      menosLleva_robin_glob[i]=robin_glo$menosLleva
      menosLleva_greedy_alea[i]=greedy_ale$menosLleva
      
      
      
      esProporcionalEst[i]=max(mio$proporcionalidad,lipton_solo$proporcionalidad,lipton$proporcionalidad,greedy_solo$proporcionalidad,robin_solo$proporcionalidad,robin_salt_solo$proporcionalidad,greedy_glo$proporcionalidad,robin_glo$proporcionalidad,greedy_ale$proporcionalidad)
      
      alfa_efs=c(alfa_ef_mio[i],alfa_ef_lipton_solo[i],alfa_ef_lipton[i],alfa_ef_greedy_solo[i],alfa_ef_robin_solo[i],alfa_ef_robin_salt_solo[i],alfa_ef_greedy_glob[i],alfa_ef_robin_glob[i],alfa_ef_greedy_alea[i])
      alfa_ef1s=c(alfa_ef1_mio[i],alfa_ef1_lipton_solo[i],alfa_ef1_lipton[i],alfa_ef1_greedy_solo[i],alfa_ef1_robin_solo[i],alfa_ef1_robin_salt_solo[i],alfa_ef1_greedy_glob[i],alfa_ef1_robin_glob[i],alfa_ef1_greedy_alea[i])
      alfa_efxs=c(alfa_efx_mio[i],alfa_efx_lipton_solo[i],alfa_efx_lipton[i],alfa_efx_greedy_solo[i],alfa_efx_robin_solo[i],alfa_efx_robin_salt_solo[i],alfa_efx_greedy_glob[i],alfa_efx_robin_glob[i],alfa_efx_greedy_alea[i])
      alfa_props=c(alfa_prop_mio[i],alfa_prop_lipton_solo[i],alfa_prop_lipton[i],alfa_prop_greedy_solo[i],alfa_prop_robin_solo[i],alfa_prop_robin_salt_solo[i],alfa_prop_greedy_glob[i],alfa_prop_robin_glob[i],alfa_prop_greedy_alea[i])
      alfa_prop1s=c(alfa_prop1_mio[i],alfa_prop1_lipton_solo[i],alfa_prop1_lipton[i],alfa_prop1_greedy_solo[i],alfa_prop1_robin_solo[i],alfa_prop1_robin_salt_solo[i],alfa_prop1_greedy_glob[i],alfa_prop1_robin_glob[i],alfa_prop1_greedy_alea[i])
      alfa_propxs=c(alfa_propx_mio[i],alfa_propx_lipton_solo[i],alfa_propx_lipton[i],alfa_propx_greedy_solo[i],alfa_propx_robin_solo[i],alfa_propx_robin_salt_solo[i],alfa_propx_greedy_glob[i],alfa_propx_robin_glob[i],alfa_propx_greedy_alea[i])
      efs=c(ef_mio[i],ef_lipton_solo[i],ef_lipton[i],ef_greedy_solo[i],ef_robin_solo[i],ef_robin_salt_solo[i],ef_greedy_glob[i],ef_robin_glob[i],ef_greedy_alea[i])
      ef1s=c(ef1_mio[i],ef1_lipton_solo[i],ef1_lipton[i],ef1_greedy_solo[i],ef1_robin_solo[i],ef1_robin_salt_solo[i],ef1_greedy_glob[i],ef1_robin_glob[i],ef1_greedy_alea[i])
      efxs=c(efx_mio[i],efx_lipton_solo[i],efx_lipton[i],efx_greedy_solo[i],efx_robin_solo[i],efx_robin_salt_solo[i],efx_greedy_glob[i],efx_robin_glob[i],efx_greedy_alea[i])
      enviMaxs=c(envidiaMaxima_mio[i],envidiaMaxima_lipton_solo[i],envidiaMaxima_lipton[i],envidiaMaxima_greedy_solo[i],envidiaMaxima_robin_solo[i],envidiaMaxima_robin_salt_solo[i],envidiaMaxima_greedy_glob[i],envidiaMaxima_robin_glob[i],envidiaMaxima_greedy_alea[i])
      envis=c(envidiosos_mio[i],envidiosos_lipton_solo[i],envidiosos_lipton[i],envidiosos_greedy_solo[i],envidiosos_robin_solo[i],envidiosos_robin_salt_solo[i],envidiosos_greedy_glob[i],envidiosos_robin_glob[i],envidiosos_greedy_alea[i])
      props=c(proporcionalidad_mio[i],proporcionalidad_lipton_solo[i],proporcionalidad_lipton[i],proporcionalidad_greedy_solo[i],proporcionalidad_robin_solo[i],proporcionalidad_robin_salt_solo[i],proporcionalidad_greedy_glob[i],proporcionalidad_robin_glob[i],proporcionalidad_greedy_alea[i])
      nashs=c(bienNash_mio[i],bienNash_lipton_solo[i],bienNash_lipton[i],bienNash_greedy_solo[i],bienNash_robin_solo[i],bienNash_robin_salt_solo[i],bienNash_greedy_glob[i],bienNash_robin_glob[i],bienNash_greedy_alea[i])
      socials=c(bienSocial_mio[i],bienSocial_lipton_solo[i],bienSocial_lipton[i],bienSocial_greedy_solo[i],bienSocial_robin_solo[i],bienSocial_robin_salt_solo[i],bienSocial_greedy_glob[i],bienSocial_robin_glob[i],bienSocial_greedy_alea[i])
      menosLlevan=c(menosLleva_mio[i],menosLleva_lipton_solo[i],menosLleva_lipton[i],menosLleva_greedy_solo[i],menosLleva_robin_solo[i],menosLleva_robin_salt_solo[i],menosLleva_greedy_glob[i],menosLleva_robin_glob[i],menosLleva_greedy_alea[i])
      tiempos=c(tiempo_mio[i],tiempo_lip_solo[i],tiempo_lip[i],tiempo_greedy_solo[i],tiempo_robin_solo[i],tiempo_robin_salt_solo[i],tiempo_greedy_glob[i],tiempo_robin_glob[i],tiempo_greedy_alea[i])
      
      guardo=c(i,j,alfa_efs,alfa_ef1s,alfa_efxs,alfa_props,alfa_prop1s,alfa_propxs,efs,ef1s,efxs,enviMaxs,envis,props,nashs,socials,menosLlevan,esProporcionalEst[i],tiempos)
      guardo_inst=c(i,j,as.vector(M))
      write.table(t(guardo), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(t(guardo_inst), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      print(paste(nAgentes,"_agentes_",nObjetos,"_objetos_","lambda_",lambda,"_repeticion_", i, "_caso_", j,sep=""))
    }
  }  
}



#la siguiente funcion pasa un reparto pensado como vector
# donde la i-ésima coordenada dice quien se lleva el bien i
# a una lista  donde reparto[[j]] son los bienes que le tocan al agente j 
rep_vec_a_list = function(v_rep,n_agentes){
  reparto = vector(mode="list",n_agentes)
  n_bienes=length(v_rep)
  for(i in 1:n_bienes){
    j=v_rep[i]
    reparto[[j]]=c(reparto[[j]],i)
  }
  reparto
}

# ahora hacemos un algoritmo que, en cada paso, le entrega un bien a aquel que siente
# que menos está llevando con lo repartido hasta el momento. 
# Si todos los biene miden más que cero fijamos un orden para la primera ronda
# y luego, si hay empate de sensaciones se sortea a quien darle.
###########
# entradas:
# orden_inic: orden para la primera rueda.
# valoraciones:
# #########
# Salidas:
# reparto
###########

reparto_ultimo_elige = function(orden_inicial,valoraciones){
  n_agentes=dim(valoraciones)[2]
  n_bienes=dim(valoraciones)[1]
  bienes_disponibles=1:n_bienes
  reparto=vector(mode="list",n_agentes)
  if(n_bienes<=n_agentes){
    for(i in 1:n_bienes){
      toca=orden_inicial[i]
      deseados=which(valoraciones[bienes_disponibles,toca]==max(valoraciones[bienes_disponibles,toca]))
      if(length(deseados)==1){
        reparto[[toca]]=c(reparto[[toca]],bienes_disponibles[deseados])
        bienes_disponibles=bienes_disponibles[-deseados]
      }
      if(length(deseados)>1){
        deseado=sample(deseados,1)
        reparto[[toca]]=c(reparto[[toca]],bienes_disponibles[deseado])
        bienes_disponibles=bienes_disponibles[-deseado]
      }
    }
  }
}
#falta terminar!!!!


########################
# AHORA FUNCIONES QUE CONTEMPLEN DISTINTAS PROPORCIONES
########################

envidia_heredan_weighted=function(valoraciones,reparto,pesos){
  S=valoracionReparto(reparto,valoraciones)
  props=proporciones(valoraciones)
  k1=dim(S)[1]
  envidiaMat=envyRatioMat=matrix(,k1,k1)
  envidian=vector(,length=k1)
  for(i in 1:k1){
    envidiaMat[i,]=S[i,]/pesos-S[i,i]/pesos[i]
    envidian[i]=sum(envidiaMat[i,]>0)
    if(S[i,i]==0){envyRatioMat[i,i]=1}
    envyRatioMat[i,]=(S[i,]/pesos)/(S[i,i]/pesos[i])
    }
  maximaEnvidia=max(envidiaMat[row(envidiaMat)!=col(envidiaMat)])
  maximoEnvyRatio=max(envyRatioMat[row(envyRatioMat)!=col(envyRatioMat)])
  donde=which(envyRatioMat == maximoEnvyRatio, arr.ind = TRUE)
  masEnvidioso=donde[1]
  masEnvidiado=donde[2]
  list(envidiaMat=envidiaMat,maximoEnvyRatio=maximoEnvyRatio,maximaEnvidia=maximaEnvidia,masEnvidioso=masEnvidioso,masEnvidiado=masEnvidiado,envidian=envidian)
}



simulacionConExh = function(nRep1i,nRep1f,nRep2,nRep3,nObjetos,nAgentes,lambda){
  
  # --- 1. VECTORES VACÍOS ---
  num_cotizaciones <- (nRep1f - nRep1i + 1)
  
  # Reparto Exhaustivo que maximiza Leximin (Exh_Lex)
  alfa_ef_Exh_Lex=alfa_efX_Exh_Lex=alfa_ef1_Exh_Lex=vector(,length=num_cotizaciones)
  alfa_prop_Exh_Lex=alfa_propX_Exh_Lex=alfa_prop1_Exh_Lex=vector(,length=num_cotizaciones)
  social_Exh_Lex=nash_Exh_Lex=menosLleva_Exh_Lex=vector(,length=num_cotizaciones)
  
  # Reparto Exhaustivo que maximiza Nash (Exh_Nash)
  alfa_ef_Exh_Nash=alfa_efX_Exh_Nash=alfa_ef1_Exh_Nash=vector(,length=num_cotizaciones)
  alfa_prop_Exh_Nash=alfa_propX_Exh_Nash=alfa_prop1_Exh_Nash=vector(,length=num_cotizaciones)
  social_Exh_Nash=nash_Exh_Nash=menosLleva_Exh_Nash=vector(,length=num_cotizaciones)
  
  # "Verdad Verdadera" - Valores Óptimos (Exh_Opt)
  alfa_ef_Exh_Opt=alfa_efX_Exh_Opt=alfa_ef1_Exh_Opt=vector(,length=num_cotizaciones)
  alfa_prop_Exh_Opt=alfa_propX_Exh_Opt=alfa_prop1_Exh_Opt=vector(,length=num_cotizaciones)
  nash_Exh_Opt=menosLleva_Exh_Opt=vector(,length=num_cotizaciones)
  
  # Tiempos
  tiempo_Exh_total=vector(,length=num_cotizaciones)
  
  # --- 2. GENERACIÓN DE DATOS Y ARCHIVOS ---
  
  # Nombres de archivos (con el prefijo EXH_)
  
  
  nombre_archi=paste("reparto_EXH_",nObjetos,"_bienes_",nAgentes,"_agen_","_nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_lambda_",lambda,".txt",sep="")
  nombre_archi_inst=paste("instancia_EXH_",nObjetos,"_bienes_",nAgentes,"_agen_","_nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_lambda_",lambda,".txt",sep="")
  
  archivo_resultados <- nombre_archi
  archivo_instancia <- nombre_archi_inst
  
  # Nombres de columnas (eliminada la redundancia de menosLleva)
  ef_nombres= c("ef_Exh_Lex","ef_Exh_Nash", "ef_Exh_Opt")
  efX_nombres= c("efX_Exh_Lex","efX_Exh_Nash", "efX_Exh_Opt")
  ef1_nombres= c("ef1_Exh_Lex","ef1_Exh_Nash", "ef1_Exh_Opt")
  prop_nombres= c("prop_Exh_Lex","prop_Exh_Nash", "prop_Exh_Opt")
  propX_nombres= c("propX_Exh_Lex","propX_Exh_Nash", "propX_Exh_Opt")
  prop1_nombres= c("prop1_Exh_Lex","prop1_Exh_Nash", "prop1_Exh_Opt")
  nash_nombres= c("nash_Exh_Lex","nash_Exh_Nash", "nash_Exh_Opt")
  social_nombres= c("social_Exh_Lex","social_Exh_Nash")
  menosLleva_nombres= c("menosLleva_Exh_Lex","menosLleva_Exh_Nash") # Columna redundante eliminada
  tiempo_nombres= c("tiempo_Exh_total")
  
  columnas=c("rep","caso",ef_nombres,efX_nombres,ef1_nombres,prop_nombres,propX_nombres,prop1_nombres,nash_nombres,social_nombres,menosLleva_nombres,tiempo_nombres)
  columnas2=c("rep","caso",rep(1:nObjetos,nAgentes))
  write.table(t(columnas), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(t(columnas2), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  # SIMULACIÓN ---
  valores_c_simul=read.table("valores_c_Dirichlet2.txt",header=TRUE)
  caso=intersect(which(round(valores_c_simul$E,3)==round(1/nAgentes,3)),which(valores_c_simul$k==nObjetos))
  valor_c=valores_c_simul$c[caso]
  alfaVec=rep(valor_c,nObjetos)
  #alfaVec=rep(1,nObjetos)
  for(i in nRep1i:nRep1f){
    set.seed(500+i)
    #X1= rpareto(nObjetos,1,alfa) #Cotizaciones
    X1 <- rdirichlet(1, alfaVec)
    for(j in 1:nRep2){
      set.seed(1000+nRep2*i+j)
      M <- t(rdirichlet(nAgentes,lambda*(as.vector(X1)))) #Instancias
      
      # --- 4. BLOQUE DE EJECUCIÓN ---
      
      start_time=Sys.time()
      resultados_exhaustivos = repartoExhaustivo_v2(M) 
      end_time=Sys.time()
      tiempo_Exh_total[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # 1. Analizar "Campeón Leximin" (Exh_Lex)
      reparto_Exh_Lex = resultados_exhaustivos$reparto_max_leximin
      metricas_Exh_Lex = felicidad2(reparto_Exh_Lex, M)
      
      # 2. Analizar "Campeón Nash" (Exh_Nash)
      reparto_Exh_Nash = resultados_exhaustivos$reparto_max_nash
      metricas_Exh_Nash = felicidad2(reparto_Exh_Nash, M)
      
      
      # --- 5. BLOQUE DE GUARDADO DE DATOS (llenamos los vectores) ---
      
      # --- Métricas de Exh_Lex ---
      
      alfa_ef_Exh_Lex[i] = metricas_Exh_Lex$alfa_ef
      alfa_efX_Exh_Lex[i] = metricas_Exh_Lex$alfa_efx
      alfa_ef1_Exh_Lex[i] = metricas_Exh_Lex$alfa_ef1
      alfa_prop_Exh_Lex[i] = metricas_Exh_Lex$alfa_prop
      alfa_propX_Exh_Lex[i] = metricas_Exh_Lex$alfa_propx
      alfa_prop1_Exh_Lex[i] = metricas_Exh_Lex$alfa_prop1
      nash_Exh_Lex[i] = metricas_Exh_Lex$bienNash # <--- CORREGIDO
      social_Exh_Lex[i] = metricas_Exh_Lex$bienSocial
      menosLleva_Exh_Lex[i] = metricas_Exh_Lex$menosLleva
      
      # --- Métricas de Exh_Nash ---
      alfa_ef_Exh_Nash[i] = metricas_Exh_Nash$alfa_ef
      alfa_efX_Exh_Nash[i] = metricas_Exh_Nash$alfa_efx
      alfa_ef1_Exh_Nash[i] = metricas_Exh_Nash$alfa_ef1
      alfa_prop_Exh_Nash[i] = metricas_Exh_Nash$alfa_prop
      alfa_propX_Exh_Nash[i] = metricas_Exh_Nash$alfa_propx
      alfa_prop1_Exh_Nash[i] = metricas_Exh_Nash$alfa_prop1
      nash_Exh_Nash[i] = metricas_Exh_Nash$bienNash # <--- CORREGIDO
      social_Exh_Nash[i] = metricas_Exh_Nash$bienSocial
      menosLleva_Exh_Nash[i] = metricas_Exh_Nash$menosLleva
      
      # --- "Verdad Verdadera" (Los valores óptimos directos) ---
      alfa_ef_Exh_Opt[i] = resultados_exhaustivos$alfa_ef
      alfa_efX_Exh_Opt[i] = resultados_exhaustivos$alfa_efX
      alfa_ef1_Exh_Opt[i] = resultados_exhaustivos$alfa_ef1
      alfa_prop_Exh_Opt[i] = resultados_exhaustivos$alfa_prop
      alfa_propX_Exh_Opt[i] = resultados_exhaustivos$alfa_propX
      alfa_prop1_Exh_Opt[i] = resultados_exhaustivos$alfa_prop1
      nash_Exh_Opt[i] = resultados_exhaustivos$nashResult
      # (La línea de menosLleva_Exh_Opt fue eliminada)
      
      
      # --- 6. ESCRITURA EN ARCHIVO ---
      
      alfa_efs = c(alfa_ef_Exh_Lex[i], alfa_ef_Exh_Nash[i], alfa_ef_Exh_Opt[i])
      alfa_efXs = c(alfa_efX_Exh_Lex[i], alfa_efX_Exh_Nash[i], alfa_efX_Exh_Opt[i])
      alfa_ef1s = c(alfa_ef1_Exh_Lex[i], alfa_ef1_Exh_Nash[i], alfa_ef1_Exh_Opt[i])
      alfa_props = c(alfa_prop_Exh_Lex[i], alfa_prop_Exh_Nash[i], alfa_prop_Exh_Opt[i])
      alfa_propXs = c(alfa_propX_Exh_Lex[i], alfa_propX_Exh_Nash[i], alfa_propX_Exh_Opt[i])
      alfa_prop1s = c(alfa_prop1_Exh_Lex[i], alfa_prop1_Exh_Nash[i], alfa_prop1_Exh_Opt[i])
      nashs = c(nash_Exh_Lex[i], nash_Exh_Nash[i], nash_Exh_Opt[i])
      sociales = c(social_Exh_Lex[i], social_Exh_Nash[i])
      menosLlevan = c(menosLleva_Exh_Lex[i], menosLleva_Exh_Nash[i]) # <--- CORREGIDO
      tiempos = c(tiempo_Exh_total[i])
      
      guardo=c(i,j,alfa_efs,alfa_efXs,alfa_ef1s,alfa_props,alfa_propXs,alfa_prop1s,nashs,sociales,menosLlevan,tiempos)
      guardo_inst=c(i,j,as.vector(M))
      
      write.table(t(guardo), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(t(guardo_inst), file = archivo_instancia, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      
      
        print(paste(nAgentes,"_agentes_EXH_",nObjetos,"_objetos","_lambda_",lambda,"_repeticion_", i, "_caso_", j,sep=""))
      
      # (etc.)
      
    }
  }
}
