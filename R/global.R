library(shinydashboard)
library(rhandsontable)
library(data.table)
library(tidyverse)
library(EnvStats)
library(plotly)
library(readxl)
library(gamlss)
library(shiny)
library(qcc)
library(DT)

## Leitura dos dados ####

balneabilidade <- NULL
list <- list()
anos <- 2010:2019
sheets <- paste('Rdata', anos, sep = '')


for (i in 1:length(sheets)) {
  list[[i]] <- read_excel("Balneabilidade.xls", 
                          sheet = sheets[i]) 
}

balneabilidade <- do.call('rbind', list)

balneabilidadenatal=balneabilidade%>%
  filter(PONTO=="NA01"|PONTO=="NA02"|PONTO=="NA03"|PONTO=="NA04"|
           PONTO=="NA05"|PONTO=="NA06"|PONTO=="NA07"|PONTO=="NA08"|
           PONTO=="NA09"|PONTO=="NA10"|PONTO=="NA11"|PONTO=="NA12"|
           PONTO=="NA13"|PONTO=="NA14"|PONTO=="NA15")

attach(balneabilidadenatal)

balneabilidadenatal %>% dplyr::select(COLIFORMES,PONTO) %>% 
  group_by(PONTO) %>% summarise_at(vars("COLIFORMES"),mean)

pam=matrix(paste(PONTO,ANO,MÊS),ncol=1)
grupos=qcc.groups(COLIFORMES,pam)

pam2=paste(PONTO,ANO)
pam2=matrix(pam2[!duplicated(pam2)],ncol=1)

## Calcular quantos Rbarra estão fora dos limites ####
grafr=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dados=grupos[rownames(grupos)%in%gr,]
  amp=apply(dados,1,function(x){max(na.omit(x))-min(na.omit(x))})
  lsup=qcc(dados,"R",plot=F)$limits
  return(sum(amp>lsup[,2]))
}

apply(pam2,1,grafr)

## Banco de dados com R barra ####
grafr2=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dados=grupos[rownames(grupos)%in%gr,]
  amp=apply(dados,1,function(x){max(na.omit(x))-min(na.omit(x))})
  lsup=qcc(dados,"R",plot=F)$limits
  while(sum(amp>lsup[,2])>0){
    dados=dados[!(amp>lsup[,2]),]
    amp=apply(dados,1,function(x){max(na.omit(x))-min(na.omit(x))})
    lsup=qcc(dados,"R",plot=F)$limits
  }
  sigmahat=qcc(dados,"R",plot=F)$std
  linhas=rownames(dados)
  return(cbind(sigmahat,linhas))
}

aaa=apply(pam2,1,grafr2)
dados=melt(aaa)

dados=dados%>%
  group_by(L1)%>%
  pivot_wider(names_from=Var2,values_from=value)%>%
  ungroup()

## Calcular quantos xbarra estão fora de controle ####
dados=dados[,c(3,4)]

grupos2=grupos[rownames(grupos)%in%dados$linhas,]
grafxbar=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dad=grupos2[rownames(grupos2) %in% gr,]
  xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
  limites=qcc(dad,"xbar",plot=F)$limits
  return(sum(xbarra>limites[,2]|xbarra<limites[,1]))
}

apply(pam2,1,grafxbar)

## Banco de dados com o xbarra ####

grafxbar2=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dad=grupos2[rownames(grupos2) %in% gr,]
  xbarra=apply(dad,1,function(x)mean(x, na.rm = T))
  limites=qcc(dad,"xbar",plot=F)$limits
  while(sum(xbarra>limites[,2]|xbarra<limites[,1])>0){
    dad=dad[!(xbarra>limites[,2]|xbarra<limites[,1]),]
    xbarra=apply(dad,1,function(x)mean(x, na.rm = T))
    limites=qcc(dad,"xbar",plot=F)$limits
  }
  xbarra=mean(xbarra)
  linhas=rownames(dad)
  return(cbind(xbarra,linhas))
}

bbb=apply(pam2,1,grafxbar2)
dados2=melt(bbb)
dados2=dados2%>%
  group_by(L1)%>%
  pivot_wider(names_from=Var2,values_from=value)%>%
  ungroup()
dados2=dados2[,c(3,4)]
dadosof=merge(dados,dados2)
dadosof$sigmahat <- as.numeric(paste(dadosof$sigmahat))
dadosof$xbarra <- as.numeric(paste(dadosof$xbarra))

dadosof2=dadosof[!duplicated(dadosof$xbarra),]
dadosof2$ponto=substring(dadosof2$linhas,1,4)
dadosof2$ano=substring(dadosof2$linhas,6,9)
rownames(dadosof2)<-substring(dadosof2$linhas,1,9)
dadosof2=dadosof2[,-1]
dadosof2$sigmahat<-as.numeric(dadosof2$sigmahat)
dadosof2$xbarra<-as.numeric(dadosof2$xbarra)

## Gráfico de controle ####

grafsobcontrole=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  gr <- as.factor(gr)
  gr <- factor(gr, levels = paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")))
  
  dad=grupos2[rownames(grupos2) %in% levels(gr),]
  num <- match(levels(gr),rownames(dad))
  dad <- dad[na.omit(num),]
  dad <- as.data.frame(dad)
  dad$V1 <- as.numeric(dad$V1)
  dad$V2 <- as.numeric(dad$V2)
  dad$V3 <- as.numeric(dad$V3)
  dad$V4 <- as.numeric(dad$V4)
  dad$V5 <- as.numeric(dad$V5)
  names(dad) <- NULL
  dad <- as.matrix(dad)
  
  xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
  limites=qcc(dad,"xbar",plot=F)$limits
  while(sum(xbarra>limites[,2]|xbarra<limites[,1])>0){
    dad=dad[!(xbarra>limites[,2]|xbarra<limites[,1]),]
    xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
    limites=qcc(dad,"xbar",plot=F)$limits
    sigmahat=qcc(dad,"R",plot=F)$std
  }
  #xbarra=qcc(dad,"xbar",plot=F)$statistics
  limites=qcc(dad,"xbar",plot=F)$limits
  linhas=rownames(dad)
  sigmahat=qcc(dad,"R",plot=F)$std
  return(cbind(xbarra,linhas,sigmahat,limites))
}

zzz=apply(pam2,1,grafsobcontrole)
dados4=melt(zzz)
dados4=dados4%>%
  group_by(L1)%>%
  pivot_wider(names_from=Var2,values_from=value)%>%
  ungroup()

dados4=dados4[,c(3,4,5,6,7)]
dados4$xbarra <- as.numeric(paste(dados4$xbarra))
dados4$sigmahat <- round(as.numeric(paste(dados4$sigmahat)),2)
dados4$LCL <- ifelse(round(as.numeric(paste(dados4$LCL)),2) < 0,0,dados4$sigmahat <- round(as.numeric(paste(dados4$sigmahat)),2))
dados4$UCL <- round(as.numeric(paste(dados4$UCL)),2)

grafnaosobcontrole=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  gr <- as.factor(gr)
  gr <- factor(gr, levels = paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")))
  
  dad=grupos2[rownames(grupos2) %in% levels(gr),]
  num <- match(levels(gr),rownames(dad))
  dad <- dad[na.omit(num),]
  dad <- as.data.frame(dad)
  dad$V1 <- as.numeric(dad$V1)
  dad$V2 <- as.numeric(dad$V2)
  dad$V3 <- as.numeric(dad$V3)
  dad$V4 <- as.numeric(dad$V4)
  dad$V5 <- as.numeric(dad$V5)
  names(dad) <- NULL
  dad <- as.matrix(dad)
  
  xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
  limites=qcc(dad,"xbar",plot=F)$limits
  linhas=rownames(dad)
  sigmahat=qcc(dad,"R",plot=F)$std
  return(cbind(xbarra,linhas,sigmahat,limites))
}

yyy=apply(pam2,1,grafnaosobcontrole)
dados5=melt(yyy)
dados5=dados5 %>%
  group_by(L1)%>%
  pivot_wider(names_from=Var2,values_from=value)%>%
  ungroup()

dados5=dados5[,c(3,4,5,6,7)]
dados5$xbarra <- as.numeric(paste(dados5$xbarra))
dados5$sigmahat <- round(as.numeric(paste(dados5$sigmahat)),2)
dados5$LCL <- ifelse(round(as.numeric(paste(dados5$LCL)),2) < 0,0,dados5$sigmahat <- round(as.numeric(paste(dados5$sigmahat)),2))
dados5$UCL <- round(as.numeric(paste(dados5$UCL)),2)

dff <- dados4 %>% mutate(Praia = str_split_fixed(linhas," ", n = Inf)[,1]) %>% mutate(Ano = str_split_fixed(linhas," ", n = Inf)[,2]) %>% mutate(Mes = str_split_fixed(linhas," ", n = Inf)[,3])
dff$Praia <- as.factor(dff$Praia)
dff$Ano <- as.factor(dff$Ano)
dff$Mes <- as.factor(dff$Mes)
dff$Mes <- factor(dff$Mes, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))

dff1 <- dados5 %>% mutate(Praia = str_split_fixed(linhas," ", n = Inf)[,1]) %>% mutate(Ano = str_split_fixed(linhas," ", n = Inf)[,2]) %>% mutate(Mes = str_split_fixed(linhas," ", n = Inf)[,3])
dff1$Praia <- as.factor(dff1$Praia)
dff1$Ano <- as.factor(dff1$Ano)
dff1$Mes <- as.factor(dff1$Mes)
dff1$Mes <- factor(dff1$Mes, levels = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ"))


