balneabilidade <- read.csv("teste_balneabilidade.csv",sep = ";")

attach(balneabilidade)

# função para checar a qualidade da agua na semana da coleta (NAO TERMINADA) --------------

check <- function(vetor) {
  
  prop <- NULL
  
  temp <- vetor < 250
  
  for (i in 5:length(vetor)) {
    
    prop[i] <- ifelse(sum(temp[i-4:i]) > 3, 'p', 'np')
  }
  
  prop
  
}

check(COLIFORMES)

# função para pegar apenas pontos específicos por ano-----------------------------
#essa função apenas serve para criar o gráfico de controle (I etapa), descosiderando os pontos fora dos limites até não haverem mais

praia <- function(ano, ponto) {
  balneabilidade %>% 
    filter(ANO == ano, PONTO == ponto) %>% 
    select(data = DATA, coliformes = COLIFORMES)
}

# função para criar graficos de controle da amplitude movel ---------------

controle.amplitude <- function(data) {
  
  #data <- pull(data)
  n <- length(data)
  mr <- matrix(c(data[1:n-1],data[2:n]), ncol = 2)
  
  grafico.controle.mr <- qcc::qcc(mr, type = 'R', plot = FALSE)
  
  while (length(grafico.controle.mr$violations$beyond.limits) != 0) {
    
    index <- grafico.controle.mr$violations$beyond.limits
    
    mr <- mr[-index,]
    
    grafico.controle.mr <- qcc::qcc(mr, type = 'R', plot = FALSE)
    
    index <- NULL
    
  }
  
  grafico.controle.mr
  
}

# função para criar os graficos de controle da media -------------------------------
library(qcc)

controle.media <- function(variaveis) {
  
  init <- controle.amplitude(variaveis)
  
  variaveis <- unname(c(init$data[,1], init$data[dim(init$data)[1],2]))
  
  grafico.controle.media <- qcc::qcc(variaveis, type = 'xbar.one',
                               ylim = c(0, max(variaveis)),
                               plot = FALSE)
  
  fora.limite <- NULL
  
  while (length(grafico.controle.media$violations$beyond.limits) != 0) {
    
    novos.dados <- subset(variaveis, variaveis < grafico.controle.media$limits[2])
    
    grafico.controle.media <- qcc::qcc(novos.dados, type = 'xbar.one',
                                 ylim = c(0, max(novos.dados)*1.25),
                                 plot = FALSE)
    
  }
  
  grafico.controle.media
  
}

controles <- NULL

for (i in as.numeric(names(table(ANO)))) {
  for (j in names(table(PONTO)))
  {
    control <- controle.media(praia(ano = i, ponto = j)[,2])
    
    temp <- data.frame(Ponto = j, Ano = i,
                       LIC = 0, LSC = control$limits[2],
                       `Média observada` = control$center,
                       `SD Observado` = control$std.dev)
    
    controles <- rbind(controles, temp)
  }
}

names(controles)[5] <- "media_obs"
a <- controle.media(praia(ano = 2010, ponto = "EX01")[,2])

ggplot()+
  geom_point(aes(y = as.data.frame(a[[5]])$"a[[5]]", x = 1:length(as.data.frame(a[[5]])$"a[[5]]")))+
  geom_line(aes(y = as.data.frame(a[[5]])$"a[[5]]", x = 1:length(as.data.frame(a[[5]])$"a[[5]]")))+
  geom_hline(yintercept = controles$LIC[1], col = "red", linetype = 2)+
  geom_hline(yintercept = controles$LSC[1], col = "red", linetype = 2)+
labs(x = "Observações", y = "Estatística", title = "Gráfico de controle EX01")+
  theme_light()
