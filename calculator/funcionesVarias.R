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
  list(ef1=ef1,efx=efx,menosLleva=menosLleva,proporcionalidad=proporcionalidad,envidiosos=envidiosos,envidiadosLista=envidiadosLista,envidiadosCant=envidiadosCant,envidiaMaxima=envidiaMaxima,bienSocial=bienSocial,bienNash=bienNash)
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






############
# Idea: Haré una función de reaparto intentando que la máxima envidia sea lo más
# pequeña posible. 
# paso 1: Repartir cada objeto a quien mejor lo valúa.
# paso 2: detectar el más envidioso (e1) y el más envidiado (e2).
# paso 3: detectar el bien menos valuado por e1 de los bienes que tiene e2
# paso 4: siempre según la valuación de e1: ordenar (de menor a mayor) con quién debería itercambiar
# dicho bien e2 para disminuirle el total a e2 lo mínimo posible (usar función sumaCerca)
# Probar si haciendo dicho intercambio con el que menos disminuye efectivamente baja la envidiaMaxima (función envidia)
# Si no baja, seguir probando en orden decreciente. Apenas logro bajar la envidia, repito desde paso 2.
############
# no da muy bien este repartoAgus porque parece preferible darle al envidioso que quitarle al envidiado
# p`robaré con eso en la funcion repartoAgus2
repartoAgus = function(valoraciones,k){
  n=dim(valoraciones)[1]
  if(k != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  Props=proporciones(valoraciones)

  #crearemos una lista para saber los art?culos que le asignamos a cada heredero
  #La primera fila de la lista ser?n los art?culos que se lleva el primer heredero
  #y as? siguiendo.

  art=list()
  for(i in 1:k){
    art[[i]]=which(Props[,i]==apply(Props,1,max))
  }
  repartido=art
  # hasta acá le asigné cada artículo a quien más lo valoró
  envidiaMaxiMin=0
  envidiaMax=1
  while(envidiaMaxMin<envidiaMax){
    aa=envidia(valoraciones,repartido)
    envidiaMax=aa$maximaEnvidia
    envidioso=aa$masEnvidioso
    envidiado=aa$masEnvidiado
    bb=valoraciones[repartido[[envidiado]],envidioso]
    nbb=length(bb)
    #diferencia=1
    #diferencias=matrix(,nbb,k-1)
    envidiaMaxMat=matrix(1,nbb,k)
    for (i in 1:nbb){ # i es el articulo que entrega el envidiado
      for(j in (1:k)[-envidiado]){ #j es con quien intercambia el envidiado
        cc=sumaCerca(valoraciones[repartido[[j]],envidioso],bb[i])
        posiciones=cc$posiciones #son las posiciones de los artículos que entrega j de su vianda
        nj=length(repartido[[j]])
        rangos = nj+1-rank(valoraciones[repartido[[j]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
        intercambio = which(rangos%in%cc$posiciones) #elementos que da j al envidiado
        repartidoAux=repartido
        repartidoAux[[envidiado]]=repartidoAux[[envidiado]][-i]  # le saco el articulo i al envidiado
        repartidoAux[[envidiado]]=c(repartidoAux[[envidiado]],repartidoAux[[j]][intercambio]) #le doy los articulos de j que intercambia al envidiado
        if(length(intercambio)>0){
          repartidoAux[[j]]=repartidoAux[[j]][-intercambio] # le saco a j los articulos que intercambia
        }
        repartidoAux[[j]]=c(repartidoAux[[j]],repartido[[envidiado]][i]) # le doy a j el artículo del envidiado
        envidiaMaxMat[i,j]=envidia(valoraciones,repartidoAux)$maximaEnvidia
        #ordenada de mayor a menor segun la valoración del envidioso
      }
    }
    envidiaMaxMin=min(envidiaMaxMat)
    if(envidiaMaxMin<envidiaMax){
      filaCol=which(envidiaMaxMat==envidiaMaxMin,arr.ind = T)
      i=filaCol[1,1]
      j=filaCol[1,2]
      cc=sumaCerca(valoraciones[repartido[[j]],envidioso],bb[i])
      posiciones=cc$posiciones #son las posiciones de los artículos que entrega j de su vianda
      nj=length(repartido[[j]])
      rangos = nj+1-rank(valoraciones[repartido[[j]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
      intercambio = which(rangos%in%cc$posiciones) #elementos que da j al envidiado
      repartidoAux=repartido
      repartidoAux[[envidiado]]=repartidoAux[[envidiado]][-i]  # le saco el articulo i al envidiado
      repartidoAux[[envidiado]]=c(repartidoAux[[envidiado]],repartidoAux[[j]][intercambio]) #le doy los articulos de j que intercambia al envidiado
      if(length(intercambio)>0){
        repartidoAux[[j]]=repartidoAux[[j]][-intercambio] # le saco a j los articulos que intercambia
      }
      repartidoAux[[j]]=c(repartidoAux[[j]],repartido[[envidiado]][i]) # le doy a j el artículo del envidiado
      repartido=repartidoAux
      #print(repartido)
    }    
  }
  list(repartido=repartido,envidia=aa)
}
  


repartoAgus2 = function(valoraciones,k){
  n=dim(valoraciones)[1]
  if(k != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  Props=proporciones(valoraciones)
  
  #crearemos una lista para saber los art?culos que le asignamos a cada heredero
  #La primera fila de la lista ser?n los art?culos que se lleva el primer heredero
  #y as? siguiendo.
  
  art=list()
  for(i in 1:k){
    art[[i]]=which(Props[,i]==apply(Props,1,max))
  }
  repartido=art
  # hasta acá le asigné cada artículo a quien más lo valoró
  envidiaMaxMin=0
  envidiaMax=1
  while(envidiaMaxMin<envidiaMax){
    envidiaMaxMin=envidiaMax
    aa=envidia(valoraciones,repartido)
    envidiaMax=aa$maximaEnvidia
    envidioso=aa$masEnvidioso # a este le queremos mejorar sus bienes
    # revisaremos todos los bienes que tienen otros y los intercambiamos con el
    # envidioso y vemos cuál intercambio disminuye la envidia global lo máximo
    # posible.
    #envidiaMaxLista=list()
    for(i in (1:k)[-envidioso]){
      ni=length(repartido[[i]])
      if(ni>0){
        for (j in 1:ni){
          cc=sumaCerca(valoraciones[repartido[[envidioso]],envidioso],valoraciones[repartido[[i]][j],envidioso])
          posiciones=cc$posiciones #son las posiciones de los artículos que entrega el envidioso de su vianda
          nEnv=length(repartido[[envidioso]])
          rangos = nEnv+1-rank(valoraciones[repartido[[envidioso]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
          intercambio = which(rangos%in%cc$posiciones) #elementos que da el envidioso a i
          repartidoAux=repartido
          repartidoAux[[i]]=repartidoAux[[i]][-j]  # le saco el articulo j a i
          repartidoAux[[i]]=c(repartidoAux[[i]],repartidoAux[[envidioso]][intercambio]) #le doy los articulos a i del envidioso que intercambia
          if(length(intercambio)>0){
            repartidoAux[[envidioso]]=repartidoAux[[envidioso]][-intercambio] # le saco al envidioso los articulos que intercambia
          }
          repartidoAux[[envidioso]]=c(repartidoAux[[envidioso]],repartido[[i]][j]) # le doy al envidioso el artículo j de i
          envidiaYa=envidia(valoraciones,repartidoAux)$maximaEnvidia
          #envidiaMaxLista[[i]]=c(envidiaMaxLista[[i]],envidiaYa)
          if(envidiaYa<envidiaMax){
            envidiaMaxMin=envidiaYa
            recibeEnvidioso=c(i,j) # recibe del iésimo su jésimo artículo
          }
          #ordenada de mayor a menor segun la valoración del envidioso         
        }        
      }
    }
    
    if(envidiaMaxMin<envidiaMax){
      
      i=recibeEnvidioso[1]
      j=recibeEnvidioso[2]
      
      cc=sumaCerca(valoraciones[repartido[[envidioso]],envidioso],valoraciones[repartido[[i]][j],envidioso])
      posiciones=cc$posiciones #son las posiciones de los artículos que entrega el envidioso de su vianda
      nEnv=length(repartido[[envidioso]])
      rangos = nEnv+1-rank(valoraciones[repartido[[envidioso]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
      intercambio = which(rangos%in%cc$posiciones) #elementos que da el envidioso a i
      repartidoAux=repartido
      repartidoAux[[i]]=repartidoAux[[i]][-j]  # le saco el articulo j a i
      repartidoAux[[i]]=c(repartidoAux[[i]],repartidoAux[[envidioso]][intercambio]) #le doy los articulos a i del envidioso que intercambia
      if(length(intercambio)>0){
        repartidoAux[[envidioso]]=repartidoAux[[envidioso]][-intercambio] # le saco al envidioso los articulos que intercambia
      }
      repartidoAux[[envidioso]]=c(repartidoAux[[envidioso]],repartido[[i]][j]) # le doy al envidioso el artículo j de i
      repartido=repartidoAux
      #print(repartido)    
    }
  }
  list(repartido=repartido,envidia=aa)
}


################
# ahora quiero seguir intercambiando con el envidioso, darle algo y que entregue algo
# eligiendo para intercambiar aquel agente y aquel bien
# para los cuales baja la envidia máxima y el bien social se mantiene lo más alto posible
##################3

#aun no funciona bien
repartoAgus3 = function(valoraciones,k){
  n=dim(valoraciones)[1]
  if(k != dim(valoraciones)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  Props=proporciones(valoraciones)
  
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
  
  repartido=art
  # hasta acá le asigné cada artículo a quien más lo valoró
  envidiaMaxMin=0
  envidiaMax=1
  while(envidiaMaxMin<envidiaMax){
    envidiaMaxMin=envidiaMax
    aa=envidia(valoraciones,repartido)
    envidiaMax=aa$maximaEnvidia
    envidioso=aa$masEnvidioso # a este le queremos mejorar sus bienes
    # revisaremos todos los bienes que tienen otros y los intercambiamos con el
    # envidioso y vemos cuál intercambio disminuye la envidia global lo máximo
    # posible.
    #envidiaMaxLista=list()
    indicesBajaEnvidia=bienSocial=matrix(0,k,n)
    for(i in (1:k)[-envidioso]){
      ni=length(repartido[[i]])
      if(ni>0){
        for (j in 1:ni){
          cc=sumaCerca(valoraciones[repartido[[envidioso]],envidioso],valoraciones[repartido[[i]][j],envidioso])
          posiciones=cc$posiciones #son las posiciones de los artículos que entrega el envidioso de su vianda
          nEnv=length(repartido[[envidioso]])
          rangos = nEnv+1-rank(valoraciones[repartido[[envidioso]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
          intercambio = which(rangos%in%cc$posiciones) #elementos que da el envidioso a i
          repartidoAux=repartido
          repartidoAux[[i]]=repartidoAux[[i]][-j]  # le saco el articulo j a i
          repartidoAux[[i]]=c(repartidoAux[[i]],repartidoAux[[envidioso]][intercambio]) #le doy los articulos a i del envidioso que intercambia
          if(length(intercambio)>0){
            repartidoAux[[envidioso]]=repartidoAux[[envidioso]][-intercambio] # le saco al envidioso los articulos que intercambia
          }
          repartidoAux[[envidioso]]=c(repartidoAux[[envidioso]],repartido[[i]][j]) # le doy al envidioso el artículo j de i
          envidiaYa=envidia(valoraciones,repartidoAux)$maximaEnvidia
          valoracion=valoracionReparto(repartidoAux,valoraciones)
          #envidiaMaxLista[[i]]=c(envidiaMaxLista[[i]],envidiaYa)
          if(envidiaYa<envidiaMax){
            indicesBajaEnvidia[i,j]=1
            bienSocial[i,j]=sum(diag(valoracion))
            #recibeEnvidioso=c(i,j) # recibe del iésimo su jésimo artículo
          }
          #ordenada de mayor a menor segun la valoración del envidioso         
        }        
      }
    }
    
    if(max(bienSocial)>0){
      bb=which(bienSocial==max(bienSocial),arr.ind = TRUE)
      i=bb[1]
      j=bb[2]
      cc=sumaCerca(valoraciones[repartido[[envidioso]],envidioso],valoraciones[repartido[[i]][j],envidioso])
      posiciones=cc$posiciones #son las posiciones de los artículos que entrega el envidioso de su vianda
      nEnv=length(repartido[[envidioso]])
      rangos = nEnv+1-rank(valoraciones[repartido[[envidioso]],envidioso]) #rangos de las valoraciones(el 1 es el más grande)
      intercambio = which(rangos%in%cc$posiciones) #elementos que da el envidioso a i
      repartidoAux=repartido
      repartidoAux[[i]]=repartidoAux[[i]][-j]  # le saco el articulo j a i
      repartidoAux[[i]]=c(repartidoAux[[i]],repartidoAux[[envidioso]][intercambio]) #le doy los articulos a i del envidioso que intercambia
      if(length(intercambio)>0){
        repartidoAux[[envidioso]]=repartidoAux[[envidioso]][-intercambio] # le saco al envidioso los articulos que intercambia
      }
      repartidoAux[[envidioso]]=c(repartidoAux[[envidioso]],repartido[[i]][j]) # le doy al envidioso el artículo j de i
      envidiaMaxMin=envidia(valoraciones,repartidoAux)$maximaEnvidia
      repartido=repartidoAux
      
      #print(repartido)    
      
    }
      
    
  }
  list(repartido=repartido,envidia=aa)
}

#####
# idea para mejorar la función de arriba: no sólo probar con el intercambio de un elemento con las posiciones de los que "sumanCerca" sino con toda la
# cadena de esas posiciones. Eso permitirá por ejemplo en un artículo que recibe el envidioso y que tendría que devolver 
# 2 artículos, que devuelva 0, 1 o 2 artículos y ver qué es lo que da mejor
###




roundRobin=function(valoraciones,k){
  if(k != dim(valoraciones)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  props=proporciones(valoraciones)
  nBienes=dim(valoraciones)[1]
  nAgentes=dim(valoraciones)[2]
  ordenEnQueEligen=sample(1:k,k)
  bienesSinElegir=1:nBienes
  todosRecibenMin=nBienes%/%nAgentes
  resto=nBienes-todosRecibenMin*nAgentes
  if(resto>0){
    reciben=c(rep(ordenEnQueEligen,todosRecibenMin),ordenEnQueEligen[1:resto])  
  }else{
    reciben=rep(ordenEnQueEligen,todosRecibenMin)
  }
  lleva=list()
  lleva[[k+1]]=nBienes+1
  for(i in reciben){
    lugar=which(props[bienesSinElegir,i]==max(props[bienesSinElegir,i]))
    elige=bienesSinElegir[lugar]
    bienesSinElegir=bienesSinElegir[-lugar]
    lleva[[i]]=c(lleva[[i]],elige)
  }
  lleva=lleva[-(k+1)]
  list(repartido=lleva)
}




###################
# función repartoMinEnv  intenta minimizar la envidia máxima
##################
# Lo programé pero no da muy bien todavía. Falta repensar!!!

repartoMinEnv=function(k,M){

  n=dim(M)[1]
  if(k != dim(M)[2]){stop("ojo, no coinciden el número de columnas con la cantidad de herederos")}
  Props=proporciones(M)
  
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
  for(i in 1:k){
    art[[i]]=which(Props[,i]==apply(Props,1,max))
  }
  
  #sumamos a ver cuanto siente que se lleva cada uno
  Lleva=vector(,length=k)
  for(i in 1:k){
    Lleva[i]=sum(Props[art[[i]],i])  
  }
  
  aa=envidia(M,art)
  envidiaMax=max(aa$maximaEnvidia)
  envidiaMax2=1
  envidioso=aa$masEnvidioso
  j=envidioso
  
  LlevaOrig=Lleva
  #mini=min(Lleva)  #el m?nimo entre las proporciones que sienten que se llevan los hered.
  #este es el valor que se quiere maximizar.
  #mini2=0
  # en caso que se lleve menos, intento darle a "a" 
  # el que tenga menor diferencia en favor de b
  # siempre respetando que no baje el m?nimo de los que se llevan ambos
  pasos=0 #para contar en cuantos pasos termina el algoritmo
  
  repeat{
    pasos=pasos+1
    
    
      i=1
      
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
              bb=envidia(M,art2)
              envidiaMax2=max(bb$maximaEnvidia)

              #mini2=min(Lleva2)
            }
            if((envidiaMax2<envidiaMax)||(i>(k-1)*n)){
              break
            }  # me salgo del segundo repeat si logré bajar la envidia máxima o si no lo sirvió ningún artículo
          }
          if((envidiaMax2<envidiaMax)||(i>(k-1)*n)){
            break
          }
        }
      
      
    
    #Hasta ac? intent? subir el m?nimo. si no subi? el m?nimo termina el algoritmo:
    if(envidiaMax<=envidiaMax2){
      break
    }
    #y si el m?nimo subi? actualizo los datos:
    art=art2
    Lleva=Lleva2
    envidiaMax=envidiaMax2
    envidioso=bb$masEnvidioso
    j=envidioso
  }
  #try(if(mini<1/k) stop("Un participante se lleva menos de lo que deber?a. Se recomienda repetir quitando un art?culo caro y muy ponderado por ambos")) 
  return(list(Art=art,llevan=Lleva,llevanOrig=LlevaOrig,Pasos=pasos))    
}



repartoExhaustivoEFX=function(n,k,valoraciones){
  a=sum.comb(n,k)
  a=a[!apply(a,1,is.unsorted),]
  largo=dim(a)[1]
  alfaResult=0
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
        valoracion=valoracionReparto(repartido,valoraciones) #matriz de valoracion del reparto
        alfaAux=EFX(repartido,valoraciones)$alfaMin
        if(alfaAux==1){
          repartidoResult=repartido
          alfaResult=alfaAux
          return(list(alfa=alfaResult,repartido=repartidoResult))
        }
        if(alfaAux>alfaResult){
          repartidoResult=repartido
          alfaResult=alfaAux
        }
      }
    }
  }
  return(list(alfa=alfaResult,repartido=repartidoResult))
}


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





#reparto entre 2 personas con un método EFX para valuaciones aditivas cualesquiera
# Menos mal que lo programé porque me di cuenta que no era EFX que estaba haciendo algo mal
# en la demostración


repartoOrdenadoPara2 = function(valoraciones){
  M=proporciones(valoraciones)
  nObjetos=dim(M)[1]
  maximos=apply(M[,],1,max)
  ordenEntrega=order(maximos,decreasing = TRUE)
  repartido=list()
  recibe1=c()
  recibe2=c()
  j=0 #cuenta cuanto artículos voy entregando
  for(i in ordenEntrega){
    j=j+1
    recibe=which(M[i,]==max(M[i,]))
    if(recibe==1){
      if(length(recibe1)>0){
        recibiria1=union(recibe1,i)
        if((sum(M[recibiria1,2])-min(M[recibiria1,2])) < (1-sum(M[recibiria1,2]))){
          recibe1=c(recibe1,i)
        }else{
          recibe2=c(recibe2,i)
        }
      }else{
        recibe1=i
      }
    }
    if(recibe==2){
      if(length(recibe2)>0){
        recibiria2=union(recibe2,i)
        if((sum(M[recibiria2,1])-min(M[recibiria2,1])) < (1-sum(M[recibiria2,1]))){
          recibe2=c(recibe2,i)
        }else{
          recibe1=c(recibe1,i)
        }
      }else{
        recibe2=i
      }
    }
    # if((sum(M[recibe2,2])>=1/2)&(j<nObjetos)){
    #   recibe1=c(recibe1,ordenEntrega[(j+1):nObjetos])
    #   repartido[[1]]=recibe1
    #   repartido[[2]]=recibe2
    #   return(repartido)
    # }
    # if((sum(M[recibe1,1])>=1/2)&(j<nObjetos)){
    #   recibe2=c(recibe2,ordenEntrega[(j+1):nObjetos])
    #   repartido[[1]]=recibe1
    #   repartido[[2]]=recibe2
    #   return(repartido)
    # }
  }
  repartido[[1]]=recibe1
  repartido[[2]]=recibe2
  if(EFX(repartido,M)$efx==0){
    repartido[[1]]=recibe2
    repartido[[2]]=recibe1
  }
    
  # if(EFX(repartido,M)$efx==0){
  #   ordenEntrega[c(1,2)]=ordenEntrega[c(2,1)]
  #   #maximasDif=abs(M[,1]-M[,2])
  #   #ordenEntrega=order(maximasDif,decreasing = TRUE)
  #   repartido=list()
  #   recibe1=c()
  #   recibe2=c()
  #   j=0 #cuenta cuanto artículos voy entregando
  #   for(i in ordenEntrega){
  #     j=j+1
  #     recibe=which(M[i,]==max(M[i,]))
  #     if(recibe==1){
  #       if(length(recibe1)>0){
  #         recibiria1=union(recibe1,i)
  #         if((sum(M[recibiria1,2])-min(M[recibiria1,2])) < (1-sum(M[recibiria1,2]))){
  #           recibe1=c(recibe1,i)
  #         }else{
  #           recibe2=c(recibe2,i)
  #         }
  #       }else{
  #         recibe1=i
  #       }
  #     }
  #     if(recibe==2){
  #       if(length(recibe2)>0){
  #         recibiria2=union(recibe2,i)
  #         if((sum(M[recibiria2,1])-min(M[recibiria2,1])) < (1-sum(M[recibiria2,1]))){
  #           recibe2=c(recibe2,i)
  #         }else{
  #           recibe1=c(recibe1,i)
  #         }
  #       }else{
  #         recibe2=i
  #       }
  #     }
  #   }
  #   repartido[[1]]=recibe1
  #   repartido[[2]]=recibe2
  # }
  #menosLleva=min(sum(M[repartido[[1]],1]),sum(M[repartido[[2]],2]))
  return(repartido)
}


#intento hacer ahora el reparto para 3

greedyEfxAlg=function(valuacion,nAgentes){
  v=valuacion
  nObjetos=length(v)
  suma=sum(v)
  props=v/suma
  props_ord=order(props,decreasing = TRUE)
  reciben=list()
  reciben[[nAgentes+1]]=0
  reciben[[nAgentes+1]]=c()
  ponderan=rep(0,length=nAgentes)
  
  for(j in props_ord){
    i=which(ponderan==min(ponderan))[1]
    reciben[[i]]=c(reciben[[i]],j)
    ponderan[i]=ponderan[i]+props[j]
  }
  return(list(reciben=reciben,ponderan=ponderan))
}    


repartoPara2Greedy = function(valoraciones){
  M=valoraciones
  a=M[,1]
  b=M[,2]
  repartido1=list()
  repartido2=list()
  aa=greedyEfxAlg(a,2)
  bb=greedyEfxAlg(b,2)
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
  #finalmente hago el reparto que maximiza el mínimo entre los dos repartos
  minReparto1=min(sum(M[repartido1[[1]],1]),sum(M[repartido1[[2]],2]))
  minReparto2=min(sum(M[repartido2[[1]],1]),sum(M[repartido2[[2]],2]))
  if(minReparto1>=minReparto2){
    repartido=repartido1
  }else{
    repartido=repartido2
  }
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
    if(le_toca==0){le_toca=length(agentes)}
    g_lugar=which(valoraciones[unalloc_goods,orden[le_toca]]==max(valoraciones[unalloc_goods,orden[le_toca]]))
    if(length(g_lugar)>1){
      g=unalloc_goods[sample(glugar,1)]  
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
        g=unalloc_goods[sample(glugar,1)]  
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


###########
# ahora la simulacion cuando no corro repartos exhaustivos
###########

simulacionSinExh= function(nRep1i,nRep1f,nRep2,nRep3,nObjetos,nAgentes,lambda){
  #nRep1=10
  ef1_lipton=ef1_mio=ef1_robin=ef1_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  efx_lipton=efx_mio=efx_robin=efx_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  envidiaMaxima_lipton=envidiaMaxima_mio=envidiaMaxima_robin=envidiaMaxima_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  envidiosos_lipton=envidiosos_mio=envidiosos_robin=envidiosos_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  proporcionalidad_lipton=proporcionalidad_mio=proporcionalidad_robin=proporcionalidad_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  bienSocial_lipton=bienSocial_mio=bienSocial_robin=bienSocial_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  bienNash_lipton=bienNash_mio=bienNash_robin=bienNash_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  menosLleva_lipton=menosLleva_mio=menosLleva_robin=menosLleva_lipton_solo=vector(,length=(nRep1f-nRep1i+1))
  esProporcionalEst=vector(,length=(nRep1f-nRep1i+1)) #ahora es una estimación, si encuentra algún reparto proporcional da 1
  tiempo_lip=tiempo_mio=tiempo_rob=tiempo_lip_solo=vector(,length=(nRep1f-nRep1i+1))
  #vector(,length=nRep1)
  #dif=dif2=vector(,length=nRep1)
  #nObjetos=6
  #nAgentes=3
  #nRep2=100
  #porcDiscrep=0.3
  alfaVec=rep(1,nObjetos)
  #lambda=100
  nombre_archi=paste("reparto_",nObjetos,"_bienes_",nAgentes,"_agen_","nrep1i_",nRep1i,"_nrep1f_",nRep1f,"_nrep2_",nRep2,"_nrep3_",nRep3,"_lambda_",lambda,".txt",sep="")
  # if(nAgentes==2){
  #   plaut_func=plaut_rouph_2agentes
  # }else{
  #   plaut_func=plaut_rouph_3omas
  # }
  
  archivo_resultados <- nombre_archi
  
  ef1_nombres=c("ef1_lip","ef1_mio","ef1_rob","ef1_sol")
  efx_nombres=c("efx_lip","efx_mio","efx_rob","efx_sol")
  enviMax_nombres=c("enviMax_lip","enviMax_mio","enviMax_rob","enviMax_sol")
  envis_nombres=c("envis_lip","envis_mio","envis_rob","envis_sol")
  prop_nombres=c("prop_lip","prop_mio","prop_rob","prop_sol")
  nash_nombres=c("nash_lip","nash_mio","nash_rob","nash_sol")
  social_nombres=c("social_lip","social_mio","social_rob","social_sol")  #ojo que estan invertidos social_rob y social_nas
  menosLleva_nombres=c("menosLleva_lip","menosLleva_mio","menosLleva_rob","menosLleva_sol")
  esProporcional_nombres=c("proporcional")
  tiempo_nombres=c("tiempo_lip","tiempo_mio","tiempo_rob","tiempo_sol")
  
  columnas=c("rep","caso",ef1_nombres,efx_nombres,enviMax_nombres,envis_nombres,prop_nombres,nash_nombres,social_nombres,menosLleva_nombres,esProporcional_nombres,tiempo_nombres)
  write.table(t(columnas), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  #X1 <- rdirichlet(nRep1, alfaVec)
  for(i in nRep1i:nRep1f){
    set.seed(500+i)
    X1 <- rdirichlet(1, alfaVec)
    # M=matrix(,nObjetos,nAgentes)
    for(j in 1:nRep2){
      
      M <- t(rdirichlet(nAgentes,lambda*(as.vector(X1))))
      start_time=Sys.time()
      # reparto mío  
      reparto_mio=vector("list",length=nAgentes)
      reparto_mio[[1]]=1:nObjetos
      for(l in 1:nRep3){
        reparto_aux=repartoBienes(nAgentes,M)$Art
        if(comparacion_leximin_pp(reparto_mio,reparto_aux,M)==2){
          reparto_mio=reparto_aux
        }
      }
      end_time=Sys.time()
      tiempo_mio[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      # reparto lipton  
      start_time=Sys.time()
      reparto_lipton=vector("list",length=nAgentes)
      reparto_lipton[[1]]=1:nObjetos
      for(l in 1:nRep3){
        repartido_aux=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
        if(comparacion_leximin_pp(reparto_lipton,repartido_aux,M)==2){
          reparto_lipton=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_lip[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      # reparto Plat Rouphgarden  
      
      # plaut=felicidad2(plaut_func(M),M)
      
      #reparto exhaustivo que busca asignacion EF dentro de asignaciones EFX ¿qué pasa si no hay EFX?
      # exhEFX2=felicidad2(repartoExhaustivoEFX2(nObjetos,nAgentes,M)$repartido,M)
      
      #reparto exhaustivo que maximiza el bienestar de Nash
      # exhNash=felicidad2(repartoExhaustivoEFX2(nObjetos,nAgentes,M)$repartidoNash,M)
      
      lipton=felicidad2(reparto_lipton,M)
      mio=felicidad2(reparto_mio,M)
      
      # reparto lipton solo. No hace mini simulacion para buscar el mejor.  
      start_time=Sys.time()
      reparto_lipton_solo=envy_cycle_elimination_mejor(1:nAgentes,vector(mode="list",nAgentes),1:nObjetos,M)
      end_time=Sys.time()
      tiempo_lip_solo[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      lipton_solo=felicidad2(reparto_lipton_solo,M)
      
      # reparto usando Round_robin salteador
      start_time=Sys.time()
      repartido_orig=vector("list",length=nAgentes)
      repartido_orig[[1]]=1:nObjetos
      for(l in 1:nRep2){
        orden=sample(1:nAgentes,nAgentes)
        repartido_aux=Round_Robin_salteador(1:nAgentes,vector("list",nAgentes),1:nObjetos,orden,nObjetos,M)$partial_alloc
        if(comparacion_leximin_pp(repartido_orig,repartido_aux,M)==2){
          repartido_orig=repartido_aux
        }
      }
      end_time=Sys.time()
      tiempo_rob[i]=as.numeric(difftime(end_time, start_time, units = "secs"))
      robin=felicidad2(repartido_orig,M)
      
      
      ef1_lipton[i]=lipton$ef1[[1]]
      ef1_mio[i]=mio$ef1[[1]]
      # ef1_plaut[i]=plaut$ef1[[1]]
      # ef1_exhEFX2[i]=exhEFX2$ef1[[1]]
      # ef1_exhNash[i]=exhNash$ef1[[1]]
      ef1_robin[i]=robin$ef1[[1]]
      ef1_lipton_solo[i]=lipton_solo$ef1[[1]]
      
      efx_lipton[i]=lipton$efx[[1]]
      efx_mio[i]=mio$efx[[1]]
      # efx_plaut[i]=plaut$efx[[1]]
      # efx_exhEFX2[i]=exhEFX2$efx[[1]]
      # efx_exhNash[i]=exhNash$efx[[1]]
      efx_robin[i]=robin$efx[[1]]
      efx_lipton_solo[i]=lipton_solo$efx[[1]]
      
      envidiaMaxima_lipton[i]=max(lipton$envidiaMaxima)
      envidiaMaxima_mio[i]=max(mio$envidiaMaxima)
      # envidiaMaxima_plaut[i]=max(plaut$envidiaMaxima)
      # envidiaMaxima_exhEFX2[i]=max(exhEFX2$envidiaMaxima)
      # envidiaMaxima_exhNash[i]=max(exhNash$envidiaMaxima)
      envidiaMaxima_robin[i]=max(robin$envidiaMaxima)
      envidiaMaxima_lipton_solo[i]=max(lipton_solo$envidiaMaxima)
      
      envidiosos_lipton[i]=lipton$envidiosos
      envidiosos_mio[i]=mio$envidiosos
      # envidiosos_plaut[i]=plaut$envidiosos
      # envidiosos_exhEFX2[i]=exhEFX2$envidiosos
      # envidiosos_exhNash[i]=exhNash$envidiosos
      envidiosos_robin[i]=robin$envidiosos
      envidiosos_lipton_solo[i]=lipton_solo$envidiosos
      
      proporcionalidad_lipton[i]=lipton$proporcionalidad
      proporcionalidad_mio[i]=mio$proporcionalidad
      # proporcionalidad_plaut[i]=plaut$proporcionalidad
      # proporcionalidad_exhEFX2[i]=exhEFX2$proporcionalidad
      # proporcionalidad_exhNash[i]=exhNash$proporcionalidad
      proporcionalidad_robin[i]=robin$proporcionalidad
      proporcionalidad_lipton_solo[i]=lipton_solo$proporcionalidad
      
      bienNash_lipton[i]=lipton$bienNash
      bienNash_mio[i]=mio$bienNash
      # bienNash_plaut[i]=plaut$bienNash
      # bienNash_exhEFX2[i]=exhEFX2$bienNash
      # bienNash_exhNash[i]=exhNash$bienNash
      bienNash_robin[i]=robin$bienNash
      bienNash_lipton_solo[i]=lipton_solo$bienNash
      
      bienSocial_lipton[i]=lipton$bienSocial
      bienSocial_mio[i]=mio$bienSocial
      # bienSocial_plaut[i]=plaut$bienSocial
      # bienSocial_exhEFX2[i]=exhEFX2$bienSocial
      # bienSocial_exhNash[i]=exhNash$bienSocial
      bienSocial_robin[i]=robin$bienSocial
      bienSocial_lipton_solo[i]=lipton_solo$bienSocial
      
      menosLleva_lipton[i]=lipton$menosLleva
      menosLleva_mio[i]=mio$menosLleva
      # menosLleva_plaut[i]=plaut$menosLleva
      # menosLleva_exhEFX2[i]=exhEFX2$menosLleva
      # menosLleva_exhNash[i]=exhNash$menosLleva
      menosLleva_robin[i]=robin$menosLleva
      menosLleva_lipton_solo[i]=lipton_solo$menosLleva
      
      esProporcionalEst[i]=max(lipton$proporcionalidad,mio$proporcionalidad,robin$proporcionalidad,lipton_solo$proporcionalidad)
      
      
      ef1s=c(ef1_lipton[i],ef1_mio[i],ef1_robin[i],ef1_lipton_solo[i])
      efxs=c(efx_lipton[i],efx_mio[i],efx_robin[i],efx_lipton_solo[i])
      enviMaxs=c(envidiaMaxima_lipton[i],envidiaMaxima_mio[i],envidiaMaxima_robin[i],envidiaMaxima_lipton_solo[i])
      envis=c(envidiosos_lipton[i],envidiosos_mio[i],envidiosos_robin[i],envidiosos_lipton_solo[i])
      props=c(proporcionalidad_lipton[i],proporcionalidad_mio[i],proporcionalidad_robin[i],proporcionalidad_lipton_solo[i])
      nashs=c(bienNash_lipton[i],bienNash_mio[i],bienNash_robin[i],bienNash_lipton_solo[i])
      socials=c(bienSocial_lipton[i],bienSocial_mio[i],bienSocial_robin[i],bienSocial_lipton_solo[i])
      menosLlevan=c(menosLleva_lipton[i],menosLleva_mio[i],menosLleva_robin[i],menosLleva_lipton_solo[i])
      tiempos=c(tiempo_lip,tiempo_mio,tiempo_rob,tiempo_lip_solo)
      guardo=c(i,j,ef1s,efxs,enviMaxs,envis,props,nashs,socials,menosLlevan,esProporcionalEst[i],tiempos)
      
      write.table(t(guardo), file = archivo_resultados, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      print(paste("repeticion", i, "_caso_", j,sep=""))  
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


#valoraciones = matrix(c(seq(1,17,by=2)[-9],13,1:9,seq(1,25,by=3)),9,3)
#valoraciones=matrix(c(1:10,2:11,3:12,4:13,5:14),10,5)
#valoraciones=matrix(c(1:5,5:1,2:6,6:2,3:7,7:3,4:8,8:4,5:9,9:5),10,5)
#valoraciones=proporciones(valoraciones)
n_tareas=12
n_agentes=3
cotizacion=rdirichlet(1,rep(1,n_tareas))
valoraciones=t(rdirichlet(n_agentes,as.vector(cotizacion)*1000))

reparto_0=vector(mode="list",length=dim(valoraciones)[2])
reparto_0[[1]]=1:dim(valoraciones)[1]
reparto_elegido_0=reparto_0
for(i in 1:1000){
  reparto_aux=chau_tareas_feas(valoraciones)
  if(comparacion_leximin_pp_tareas(reparto_elegido_0,reparto_aux$reparto,valoraciones)==2){
    reparto_elegido_0=reparto_aux$reparto
    valoracion_obtenida_0=reparto_aux$matriz_costo_final
  }
}
#reparto_elegido_0


reparto_1=vector(mode="list",length=dim(valoraciones)[2])
reparto_1[[1]]=1:dim(valoraciones)[1]
reparto_elegido_1=reparto_1
for(i in 1:1000){
  reparto_aux=repartoTareas(n_agentes,valoraciones)
  if(comparacion_leximin_pp_tareas(reparto_elegido_1,reparto_aux$reparto,valoraciones)==2){
    reparto_elegido_1=reparto_aux$Art
    valoracion_obtenida_1=reparto_aux$llevan
  }
}

reparto_2=vector(mode="list",length=dim(valoraciones)[2])
reparto_2[[1]]=1:dim(valoraciones)[1]
reparto_elegido_2=reparto_2
for(i in 1:1000){
  reparto_aux=chau_tareas_feas2(valoraciones)
  if(comparacion_leximin_pp_tareas(reparto_elegido_2,reparto_aux$reparto,valoraciones)==2){
    reparto_elegido_2=reparto_aux$reparto
    valoracion_obtenida_2=reparto_aux$matriz_costo_final
  }
}

cat("\n===== Burden per agent (best of 1000 runs each) =====\n")
cat("chau_tareas_feas : ", round(diag(valoracion_obtenida_0), 4), "\n")
cat("repartoTareas    : ", round(valoracion_obtenida_1, 4), "\n")
cat("chau_tareas_feas2: ", round(diag(valoracion_obtenida_2), 4), "\n")
cat("\n===== Total burden (lower = more efficient) =====\n")
cat("chau_tareas_feas : ", round(sum(diag(valoracion_obtenida_0)), 4), "\n")
cat("repartoTareas    : ", round(sum(valoracion_obtenida_1), 4), "\n")
cat("chau_tareas_feas2: ", round(sum(diag(valoracion_obtenida_2)), 4), "\n")
cat("\n===== Max burden (lower = more fair) =====\n")
cat("chau_tareas_feas : ", round(max(diag(valoracion_obtenida_0)), 4), "\n")
cat("repartoTareas    : ", round(max(valoracion_obtenida_1), 4), "\n")
cat("chau_tareas_feas2: ", round(max(diag(valoracion_obtenida_2)), 4), "\n")