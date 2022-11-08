library(readxl)
library(qcc)
library(tidyverse)
library(reshape2)
library(gamlss)

balneabilidade <- NULL
list <- list()
anos <- 2010:2018
sheets <- paste('Rdata', anos, sep = '')

for (i in 1:length(sheets)) {
  list[[i]] <- read_excel("CEP 1 - Balneabilidade.xls", 
                                     sheet = sheets[i]) 
}

balneabilidade <- do.call('rbind', list)
#save(balneabilidade, file = 'balneabilildade.RData')

balneabilidadenatal=balneabilidade%>%
  filter(PONTO=="NA01"|PONTO=="NA02"|PONTO=="NA03"|PONTO=="NA04")
attach(balneabilidadenatal)

pam=matrix(paste(PONTO,ANO,MÊS),ncol=1)
grupos=qcc.groups(COLIFORMES,pam)

pam2=paste(PONTO,ANO)
pam2=matrix(pam2[!duplicated(pam2)],ncol=1)

# Calcular quantos Rbarra estão fora dos limites
grafr=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dados=grupos[rownames(grupos)%in%gr,]
  amp=apply(dados,1,function(x){max(na.omit(x))-min(na.omit(x))})
  lsup=qcc(dados,"R",plot=F)$limits
  return(sum(amp>lsup[,2]))
}

apply(pam2,1,grafr)

# Banco de dados com R barra
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
  rbarra=qcc(dados,"R",plot=F)$center
  linhas=rownames(dados)
  return(cbind(rbarra,linhas))
}

aaa=apply(pam2,1,grafr2)
dados=melt(aaa)
dados=dados%>%
  group_by(L1)%>%
  pivot_wider(names_from=Var2,values_from=value)%>%
  ungroup()


# Calcular quantos xbarra estão fora de controle
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

# Banco de dados com o xbarra
grafxbar2=function(x){
  gr=paste(x,c("JAN","FEV","MAR","ABR","MAI","JUN","JUL",
               "AGO","SET","OUT","NOV","DEZ"))
  dad=grupos2[rownames(grupos2) %in% gr,]
  xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
  limites=qcc(dad,"xbar",plot=F)$limits
  while(sum(xbarra>limites[,2]|xbarra<limites[,1])>0){
    dad=dad[!(xbarra>limites[,2]|xbarra<limites[,1]),]
    xbarra=apply(dad,1,function(x)mean(x,na.rm = T))
    limites=qcc(dad,"xbar",plot=F)$limits
  }
  xbarra=qcc(dad,"xbar",plot=F)$center
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


dadosof2=dadosof[!duplicated(dadosof$rbarra),]
dadosof2$ponto=substring(dadosof2$linhas,1,4)
dadosof2$ano=substring(dadosof2$linhas,6,9)
rownames(dadosof2)<-substring(dadosof2$linhas,1,9)
dadosof2=dadosof2[,-1]
dadosof2$rbarra<-as.numeric(dadosof2$rbarra)
dadosof2$xbarra<-as.numeric(dadosof2$xbarra)
head(dadosof2)

# calcular ICP
LIE=0
LSE=1000
d=0

icps=function(x){
  rbarra=x[1]
  xbarra=x[2]
  sigmahat=rbarra/2.059
  cp=(LSE-LIE)/(6*sigmahat)
  cpk1=LSE-xbarra
  cpk2=xbarra-LIE
  cpk=min(cpk1,cpk2)/(3*sigmahat)
  cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
  pfe=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
  saida=cbind(cp,cpk,cpm,pfe)
  names(saida)=c("CP","CPK","CPM","PFE")
  return(saida)
}


t(apply(dadosof2[,c(1,2)],1,icps))

coliformes=COLIFORMES
names(coliformes)=paste(pam)
coliformes=coliformes[names(coliformes)%in%dados2$linhas]

# Testar normalidade
grupos3=qcc.groups(coliformes,substring(names(coliformes),1,9))
aaaa<-apply(grupos3,1,function(x){
  ks.test(x,"pnorm",mean(x,na.rm = T),
          (max(x,na.rm = T)-min(x,na.rm=T))/2.059
          )$p
  })
sum(aaaa>0.05)
sum(aaaa>0.01)
sum(aaaa>10^(-6))


# Testar Inversa Gaussiana Generalizada nos sob controle
GIG("identity","identity")
aaaaa<-apply(grupos3,1,function(x){
  ks.test(x,"pGIG",mu=(mean(x,na.rm = T)),
          sigma=(max(x,na.rm = T)-min(x,na.rm=T))/2.059,
          nu=0.5)$p
  })
sum(aaaaa>0.05)
sum(aaaaa>0.01)

icps2=function(x){
  rbarra=x[1]
  xbarra=x[2]
  sigmahat=rbarra/2.059
  cp=(LSE-LIE)/(6*sigmahat)
  cpk1=LSE-xbarra
  cpk2=xbarra-LIE
  cpk=min(cpk1,cpk2)/(3*sigmahat)
  cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
  pfeNORM=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
  pfeGIG=100*pGIG(LSE,mu=xbarra,sigma=sigmahat,nu=0.5,lower.tail = F)
  saida=cbind(cp,cpk,cpm,pfeNORM,pfeGIG)
  names(saida)=c("CP","CPK","CPM","PFENORM","PFEGIG")
  return(saida)
}

t(apply(dadosof2[,c(1,2)],1,icps2))



# Testar IGG em toda amostra
grupos4=qcc.groups(COLIFORMES,paste(PONTO,ANO))
primeiro=apply(grupos4,1,function(x){
  ks.test(x,"pGIG",mu=(mean(x,na.rm = T)),
          sigma=(max(x,na.rm = T)-min(x,na.rm=T))/2.059,nu=0.5
          )$p
})
sum(primeiro>0.05)
sum(primeiro>0.01)
sum(primeiro>0.001)
