#procesamiento de datos
library(knitr)
nObjetos=c(10,15,20,25)
nAgentes=c(4)
lambda=c(10,100,1000)
setwd("~/personalrepos/division-de-tareas/calculator")
# "./simulacionExhaustivosPrueba1Lote1-500"
# "./simulacionNoExhaPrueba1Lote1-500"

# We need to run the combination of all these 36 agentes_tareas_lambda tuples
lambdas = c(10, 100, 1000)
combinaciones = data.frame(
  agentes = c(2, 2,  2,  2, 3, 3, 3, 3, 4, 4, 4, 4),
  tareas  = c(6, 9, 12, 15, 6, 7, 8, 9, 5, 6, 7, 8)
)


carpetaResultados = "./resultadosPrueba1"
dir.create(carpetaResultados, showWarnings = FALSE)

sufijo = function(tareas, agentes, lambda, prefijo) {
  paste0(prefijo, tareas, "_tareas_", agentes,
         "_agen_nrep1i_1_nrep1f_500_ninst_10_lambda_", lambda)
}

for (i in 1:nrow(combinaciones)) {
  agentes = combinaciones$agentes[i]
  tareas = combinaciones$tareas[i]
  for (lambda in lambdas) {
    archivoExhaustivo = paste0("simulacionExhaustivosPrueba1Lote1-500/",
                               sufijo(tareas, agentes, lambda, "reparto_TAREAS_EXH_"), ".txt")
    tablaExhaustivo = read.table(archivoExhaustivo, header = TRUE, sep = "\t")

    archivoNoExhaustivo = paste0("simulacionNoExhaPrueba1Lote1-500/",
                                 sufijo(tareas, agentes, lambda, "reparto_TAREAS_"), ".txt")
    tablaNoExhaustivo = read.table(archivoNoExhaustivo, header = TRUE, sep = "\t")

    masCarga_Opt=tablaExhaustivo$alfa_prop_Exh_Opt/agentes
    masCarga_Lex=tablaExhaustivo$alfa_prop_Exh_Lex/agentes
    masCarga_Nash=tablaExhaustivo$alfa_prop_Exh_Nash/agentes

    masCarga_chau=tablaNoExhaustivo$alfa_prop_chau/agentes
    masCarga_mio=tablaNoExhaustivo$alfa_prop_mio/agentes
    masCarga_chau2=tablaNoExhaustivo$alfa_prop_chau2/agentes
    masCarga_tt=tablaNoExhaustivo$alfa_prop_tt/agentes
    masCarga_tt_rnd=tablaNoExhaustivo$alfa_prop_tt_rnd/agentes
    masCarga_tt_rnd_lec=tablaNoExhaustivo$alfa_prop_tt_rnd_lec/agentes
    masCarga_chau2_rnd=tablaNoExhaustivo$alfa_prop_chau2_rnd/agentes
    masCarga_chau2_alfaef=tablaNoExhaustivo$alfa_prop_chau2_alfaef/agentes
    masCarga_round_robin=tablaNoExhaustivo$alfa_prop_round_robin/agentes
    masCarga_greedy=tablaNoExhaustivo$alfa_prop_greedy/agentes

    nombre_archivo=paste0(carpetaResultados, "/boxplots_",
                          sufijo(tareas, agentes, lambda, "reparto_TAREAS_"), ".pdf")
    nombres=c("opt", "lexi", "nash", "GRHC", "ILOSD", "GRHC2", "TTECE", "TTECER", "TTECERLEV", "GRHC2R", "GRHC2A","RR", "GRE")
    pdf(nombre_archivo, width = 8, height = 8)
    par(mar = c(6, 4, 2, 1))
    #boxplot(pla_menos, mio_menos,lip_menos,efx_menos,nash_menos,rob_menos,sol_menos,names=c("Plaut","Nuevo","lipton","Exhaus","Nash","Robin","Lipton solo"))
    boxplot(masCarga_Opt,masCarga_Lex,masCarga_Nash,masCarga_chau,masCarga_mio,masCarga_chau2,masCarga_tt,masCarga_tt_rnd,masCarga_tt_rnd_lec,masCarga_chau2_rnd,masCarga_chau2_alfaef,masCarga_round_robin,masCarga_greedy,
            names=nombres, xaxt="n")
    axis(1, at = seq_along(nombres), labels = nombres, las = 2, cex.axis = 0.8, gap.axis = -1)
    abline(h = 1/agentes, lty = 2, col = "red", lwd = 2)
    dev.off()
  }
}

# HASTA ACÁ LA GENERACION DE BOXPLOT PARA EXHAUSTIVOS


# mean(tablaExhaustivo$alfa_ef_Exh_Opt<=1)

nAgentes=4
nObjetos=c(5,6,7,8)
lambda=c(100,1000,10000)
for(nAg in nAgentes){
  for(nOb in nObjetos){
    for(lam in lambda){
      #setwd("~/Dropbox/Resultados_sin_exh")
      #setwd("./result")
      setwd("~/Dropbox/Divulgacion/division de bienes/Reparto herencia/simul/simul_2025_2")
      setwd("./resultExh")
      nombre_archi_1=paste("reparto_EXH_",nOb,"_bienes_",nAg,"_agen__","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_lambda_",lam,".txt",sep="")
      b=read.table(nombre_archi_1, header = TRUE, sep = "\t")
      setwd("../")
      setwd("./result")
      nombre_archi_2=paste("reparto_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_nrep3_500_","lambda_",lam,".txt",sep="")
      a=read.table(nombre_archi_2, header = TRUE, sep = "\t")
      
      opt_menos=b$menosLleva_Exh_Lex
      nash_menos=b$menosLleva_Exh_Nash
    
      mio_menos=a$menosLleva_mio
      lip_menos=a$menosLleva_lip
      lip_sol_menos=a$menosLleva_lip_sol
      gre_glo_menos=a$menosLleva_gre_glo
      gre_sol_menos=a$menosLleva_gre_sol
      rob_salt_menos=a$menosLleva_rob_glo
      rob_salt_sol_menos=a$menosLleva_rob_salt_sol
      rob_sol_menos=a$menosLleva_rob_sol
      
      nombre_archivo=paste("Boxplots_menos_lleva_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_",20,"_nrep3_",500,"_lambda_",lam,".pdf",sep="")  
      setwd("../")
      setwd("./proces")
      pdf(nombre_archivo, width = 8, height = 8)
      #boxplot(pla_menos, mio_menos,lip_menos,efx_menos,nash_menos,rob_menos,sol_menos,names=c("Plaut","Nuevo","lipton","Exhaus","Nash","Robin","Lipton solo"))
      boxplot(opt_menos,nash_menos,mio_menos,lip_menos,lip_sol_menos,gre_glo_menos,gre_sol_menos,rob_salt_menos,rob_salt_sol_menos,rob_sol_menos,names=c("opt","nash","New","GLip","SLip","GGre","SGre","GRRS","SRRS","SRR"))
      abline(h = 1/nAg, lty = 2, col = "red", lwd = 2)
      dev.off()
    }  
  }
}

# 3 agentes
nAgentes=3
nObjetos=c(6,7,8,9)
lambda=c(100,1000,10000)
for(nAg in nAgentes){
  for(nOb in nObjetos){
    for(lam in lambda){
      #setwd("~/Dropbox/Resultados_sin_exh")
      #setwd("./result")
      setwd("~/Dropbox/Divulgacion/division de bienes/Reparto herencia/simul/simul_2025_2")
      setwd("./resultExh")
      nombre_archi_1=paste("reparto_EXH_",nOb,"_bienes_",nAg,"_agen__","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_lambda_",lam,".txt",sep="")
      b=read.table(nombre_archi_1, header = TRUE, sep = "\t")
      setwd("../")
      setwd("./result")
      nombre_archi_2=paste("reparto_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_nrep3_500_","lambda_",lam,".txt",sep="")
      a=read.table(nombre_archi_2, header = TRUE, sep = "\t")
      
      opt_menos=b$menosLleva_Exh_Lex
      nash_menos=b$menosLleva_Exh_Nash
      
      mio_menos=a$menosLleva_mio
      lip_menos=a$menosLleva_lip
      lip_sol_menos=a$menosLleva_lip_sol
      gre_glo_menos=a$menosLleva_gre_glo
      gre_sol_menos=a$menosLleva_gre_sol
      rob_salt_menos=a$menosLleva_rob_glo
      rob_salt_sol_menos=a$menosLleva_rob_salt_sol
      rob_sol_menos=a$menosLleva_rob_sol
      
      nombre_archivo=paste("Boxplots_menos_lleva_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_",20,"_nrep3_",500,"_lambda_",lam,".pdf",sep="")  
      setwd("../")
      setwd("./proces")
      pdf(nombre_archivo, width = 8, height = 8)
      #boxplot(pla_menos, mio_menos,lip_menos,efx_menos,nash_menos,rob_menos,sol_menos,names=c("Plaut","Nuevo","lipton","Exhaus","Nash","Robin","Lipton solo"))
      boxplot(opt_menos,nash_menos,mio_menos,lip_menos,lip_sol_menos,gre_glo_menos,gre_sol_menos,rob_salt_menos,rob_salt_sol_menos,rob_sol_menos,names=c("opt","nash","New","GLip","SLip","GGre","SGre","GRRS","SRRS","SRR"))
      abline(h = 1/nAg, lty = 2, col = "red", lwd = 2)
      dev.off()
    }  
  }
}


# 2 agentes

nAgentes=2
nObjetos=c(6,9,12,15)
lambda=c(100,1000,10000)
for(nAg in nAgentes){
  for(nOb in nObjetos){
    for(lam in lambda){
      #setwd("~/Dropbox/Resultados_sin_exh")
      #setwd("./result")
      setwd("~/Dropbox/Divulgacion/division de bienes/Reparto herencia/simul/simul_2025_2")
      setwd("./resultExh")
      nombre_archi_1=paste("reparto_EXH_",nOb,"_bienes_",nAg,"_agen__","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_lambda_",lam,".txt",sep="")
      b=read.table(nombre_archi_1, header = TRUE, sep = "\t")
      setwd("../")
      setwd("./result")
      nombre_archi_2=paste("reparto_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_20_nrep3_500_","lambda_",lam,".txt",sep="")
      a=read.table(nombre_archi_2, header = TRUE, sep = "\t")
      
      opt_menos=b$menosLleva_Exh_Lex
      nash_menos=b$menosLleva_Exh_Nash
      
      mio_menos=a$menosLleva_mio
      lip_menos=a$menosLleva_lip
      lip_sol_menos=a$menosLleva_lip_sol
      gre_glo_menos=a$menosLleva_gre_glo
      gre_sol_menos=a$menosLleva_gre_sol
      rob_salt_menos=a$menosLleva_rob_glo
      rob_salt_sol_menos=a$menosLleva_rob_salt_sol
      rob_sol_menos=a$menosLleva_rob_sol
      
      nombre_archivo=paste("Boxplots_menos_lleva_",nOb,"_bienes_",nAg,"_agen_","nrep1i_",1,"_nrep1f_",500,"_nrep2_",20,"_nrep3_",500,"_lambda_",lam,".pdf",sep="")  
      setwd("../")
      setwd("./proces")
      pdf(nombre_archivo, width = 8, height = 8)
      #boxplot(pla_menos, mio_menos,lip_menos,efx_menos,nash_menos,rob_menos,sol_menos,names=c("Plaut","Nuevo","lipton","Exhaus","Nash","Robin","Lipton solo"))
      boxplot(opt_menos,nash_menos,mio_menos,lip_menos,lip_sol_menos,gre_glo_menos,gre_sol_menos,rob_salt_menos,rob_salt_sol_menos,rob_sol_menos,names=c("opt","nash","New","GLip","SLip","GGre","SGre","GRRS","SRRS","SRR"))
      abline(h = 1/nAg, lty = 2, col = "red", lwd = 2)
      dev.off()
    }  
  }
}
