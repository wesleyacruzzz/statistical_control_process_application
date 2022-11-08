shinyServer(function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    values <- reactiveValues()
    
    ## Dados ####
    dados44 <- reactive({
        
        bh <- input$bh
        ah <- input$ah
        
        dfk <- dff %>% filter(Praia %in% bh & Ano %in% ah)
        dfk <- dfk %>% group_by(Ano) %>% mutate(UCLm = mean(UCL))
        
        dfkm <- dfk[,c(1,7,8)]
        dfkl <- dfk[,c(4,7,8)]
        dfku <- dfk[,c(9,7,8)]
        
        dfkm <- gather(dfkm,"x","media",-c(Ano,Mes))
        dfkl <- gather(dfkl,"x","lcl",-c(Ano,Mes))
        dfku <- gather(dfku,"x","ucl",-c(Ano,Mes))
        dfkm$Ano_Mes <- paste(dfkm$Ano,dfkm$Mes)
        
        dfof <- inner_join(dfkm,dfkl,by = c("Ano","Mes"))
        dfof <- inner_join(dfof,dfku,by = c("Ano","Mes"))
        dfof$Ano_Mes <- factor(dfof$Ano_Mes, levels = paste(rep(unique(dfof$Ano),each = 12),c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")))
        dfof <- dfof[,-c(1,2,3,6,8)]
        texto <- ifelse(sum(dfof$media > dfof$ucl) > 0,paste0("Número de pontos fora de controle: ",
                                                                sum(dfof$media > dfof$ucl)),"Nenhum ponto fora de controle")
        coloracao <- ifelse(sum(dfof$media > dfof$ucl) > 0,"red","green")
        return(list("dfof"=dfof,"texto"=texto,"coloracao"=coloracao))
    })
    
    dados55 <- reactive({
        bh <- input$bh
        ah <- input$ah
        
        dfk1 <- dff1 %>% filter(Praia %in% bh & Ano %in% ah)
        dfk1 <- dfk1 %>% group_by(Ano) %>% mutate(UCLm = mean(UCL))
        
        dfkm1 <- dfk1[,c(1,7,8)]
        dfkl1 <- dfk1[,c(4,7,8)]
        dfku1 <- dfk1[,c(9,7,8)]
        
        dfkm1 <- gather(dfkm1,"x","media",-c(Ano,Mes))
        dfkl1 <- gather(dfkl1,"x","lcl",-c(Ano,Mes))
        dfku1 <- gather(dfku1,"x","ucl",-c(Ano,Mes))
        dfkm1$Ano_Mes <- paste(dfkm1$Ano,dfkm1$Mes)
        
        dfof1 <- inner_join(dfkm1,dfkl1,by = c("Ano","Mes"))
        dfof1 <- inner_join(dfof1,dfku1,by = c("Ano","Mes"))
        dfof1$Ano_Mes <- factor(dfof1$Ano_Mes, levels = paste(rep(unique(dfof1$Ano),each = 12),c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")))
        dfof1 <- dfof1[,-c(1,2,3,6,8)]
        texto <- ifelse(sum(dfof1$media > dfof1$ucl) > 0,paste0("Número de pontos fora de controle: ",
                                                                 sum(dfof1$media > dfof1$ucl)),"Nenhum ponto fora de controle")
        coloracao <- ifelse(sum(dfof1$media > dfof1$ucl) > 0,"red","green")
        return(list("dfof"=dfof1,"texto"=texto,"coloracao"=coloracao))
    })
    
    ## Tab 1 ####
    output$plot1 <- renderPlotly({
        
        ch <- input$ch
        
        if (ch == "Não"){
            
            dfof <- dados44()$dfof
            
            p1 <- ggplot(dfof)+
                geom_point(aes(x = Ano_Mes, y = media), col = "steelblue3")+
                geom_line(aes(x = Ano_Mes, y = media, group = 1), col = "steelblue3")+
                geom_line(aes(x = Ano_Mes, y = 0, group = 1), linetype = 2, col = "red")+
                geom_line(aes(x = Ano_Mes, y = ucl, group = 1), linetype = 2, col = "red")+
                labs(x = "Tempo", y = "Média")+
                theme_light()+
                theme(axis.text.x = element_text(angle = 90),
                      panel.grid.major = element_blank())
            
            ggplotly(p1)
            
        } else{
            
            dfof1 <- dados55()$dfof
            
            p2 <- ggplot(dfof1)+
                geom_point(aes(x = Ano_Mes, y = media), col = ifelse(dfof1$media > dfof1$ucl,"red","steelblue3"))+
                geom_line(aes(x = Ano_Mes, y = media, group = 1), col = "steelblue3")+
                geom_line(aes(x = Ano_Mes, y = 0, group = 1), linetype = 2, col = "red")+
                geom_line(aes(x = Ano_Mes, y = ucl, group = 1), linetype = 2, col = "red")+
                labs(x = "Tempo", y = "Média")+
                theme_light()+
                theme(axis.text.x = element_text(angle = 90),
                      panel.grid.major = element_blank())
            
            ggplotly(p2)
            
        }
    })
    output$info <- renderUI({
        ch <- input$ch
        
        if (ch == "Não"){
            texto <- dados44()$texto
            coloracao <- dados44()$coloracao
        } else {
            texto <- dados55()$texto
            coloracao <- dados55()$coloracao
        }
        
        
        infoBox(title = texto,color = coloracao, width = 12, fill = T)
    })
    
    ## Tab 2 ####
    output$plot2 <- renderPlotly({
        
        LIE=0
        LSE=500
        
        d=input$dh
        bh <- input$bh1
        ah <- input$ah1
        
        icps=function(x){
            x <- as.numeric(x)
            sigmahat=x[1]
            xbarra=x[2]
            cp=(LSE-LIE)/(6*sigmahat)
            cpk1=LSE-xbarra
            cpk2=xbarra-LIE
            cpk=cpk1/(3*sigmahat)
            cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
            pfe=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
            saida=cbind(cp,cpk,cpm,pfe)
            names(saida)=c("CP","CPK","CPM","PFE")
            return(saida)
        }
        
        
        coliformes=COLIFORMES
        names(coliformes)=paste(pam)
        coliformes=coliformes[names(coliformes)%in%dados2$linhas]
        
        
        grupos3=qcc.groups(coliformes,substring(names(coliformes),1,9))
        icps2=function(x){
            x <- as.numeric(x)
            sigmahat=x[1]
            xbarra=x[2]
            cp=(LSE-LIE)/(6*sigmahat)
            cpk1=LSE-xbarra
            cpk2=xbarra-LIE
            cpk=cpk1/(3*sigmahat)
            cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
            pfeNORM=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
            pfeGIG=100*pGIG(LSE,mu=xbarra,sigma=sigmahat,nu=0.5,lower.tail = F)
            saida=cbind(cp,cpk,cpm,pfeNORM,pfeGIG)
            names(saida)=c("CP","CPK","CPM","PFENORM","PFEGIG")
            return(saida)
        }
        
        icpsof = round(t(apply(dadosof2[,c(1,2)],1,icps2)),2)
        
        pfeemp=function(x){
            PFEEMP=100*(1-pemp(1000,obs=x,discrete=F))
            #PFEEMP=100*(1-pemp(1000,obs=x,discrete=T))
            return("PFEEMP"=PFEEMP)
        }
        emp=apply(grupos3,1,pfeemp)
        
        b <- round(cbind(icpsof,emp),2)
        b <- as.data.frame(b)
        b$Pontos <- rownames(b)
        b <- b %>% group_by(Pontos) %>% mutate(Praia = str_split_fixed(Pontos, " ", n = Inf)[1],
                                               Ano = str_split_fixed(Pontos, " ", n = Inf)[2])
        
        
        c <- b %>% filter(Praia %in% bh, Ano %in% ah)
        c <- c[,c(1,2,3,6,8,9)]
        names(c)[4] <- "PFE"
        
        d <- gather(c,"cp","valor",-c(Ano,Praia))
        
        p1 <- ggplot(d)+
            geom_point(aes(x = Ano, y = valor, group = Praia, col = Praia))+
            geom_line(aes(x = Ano, y = valor, group = Praia, col = Praia))+
            facet_wrap(~cp, scales = "free_y", nrow = 4)+
            labs(x = "Ano", y = " ")+
            theme_light()+
            theme(panel.grid.major = element_blank())
        
        ggplotly(p1)
        
    })
    output$dt1 <- renderDataTable({
        LIE=0
        LSE=500
        
        d=input$dh
        bh <- input$bh1
        ah <- input$ah1
        
        icps=function(x){
            x <- as.numeric(x)
            sigmahat=x[1]
            xbarra=x[2]
            cp=(LSE-LIE)/(6*sigmahat)
            cpk1=LSE-xbarra
            cpk2=xbarra-LIE
            cpk=cpk1/(3*sigmahat)
            cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
            pfe=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
            saida=cbind(cp,cpk,cpm,pfe)
            names(saida)=c("CP","CPK","CPM","PFE")
            return(saida)
        }
        
        
        coliformes=COLIFORMES
        names(coliformes)=paste(pam)
        coliformes=coliformes[names(coliformes)%in%dados2$linhas]
        
        
        grupos3=qcc.groups(coliformes,substring(names(coliformes),1,9))
        icps2=function(x){
            x <- as.numeric(x)
            sigmahat=x[1]
            xbarra=x[2]
            cp=(LSE-LIE)/(6*sigmahat)
            cpk1=LSE-xbarra
            cpk2=xbarra-LIE
            cpk=cpk1/(3*sigmahat)
            cpm=(LSE-LIE)/(6*sqrt(sigmahat^2+(d-xbarra)^2))
            pfeNORM=100*pnorm(LSE,xbarra,sigmahat,lower.tail = F)
            pfeGIG=100*pGIG(LSE,mu=xbarra,sigma=sigmahat,nu=0.5,lower.tail = F)
            saida=cbind(cp,cpk,cpm,pfeNORM,pfeGIG)
            names(saida)=c("CP","CPK","CPM","PFENORM","PFEGIG")
            return(saida)
        }
        
        icpsof = round(t(apply(dadosof2[,c(1,2)],1,icps2)),2)
        
        pfeemp=function(x){
            PFEEMP=100*(1-pemp(1000,obs=x,discrete=F))
            #PFEEMP=100*(1-pemp(1000,obs=x,discrete=T))
            return("PFEEMP"=PFEEMP)
        }
        emp=apply(grupos3,1,pfeemp)
        
        b <- round(cbind(icpsof,emp),2)
        b <- as.data.frame(b)
        b$Pontos <- rownames(b)
        
        b <- b %>% group_by(Pontos) %>% mutate(Praia = str_split_fixed(Pontos, " ", n = Inf)[1],
                                               Ano = str_split_fixed(Pontos, " ", n = Inf)[2])
        c <- b %>% filter(Praia %in% bh, Ano %in% ah)
        c <- c[,-c(4,5,8,9)]
        names(c) <- c("CP","CPK","CPM","PFE","Pontos")
        
        datatable(c, style = "bootstrap", width = "350px",autoHideNavigation = T,
                  escape = F, options = list(pageLength = 8))
    })
    ## Tab 3 ####
    
    observeEvent(input$file1, {
        
        rm(list = ls())
        inFile <- input$file1
        
        balneabilidade <- NULL
        sheets <- NULL
        
        list <- list()
        anos <- input$ai:input$af
        sheets <- paste('Rdata', anos, sep = '')
        
        values$file_name = inFile$name
        values$file_size = inFile$size
        values$file_type = inFile$type
        values$file_path = inFile$datapath
        
        for (i in 1:length(sheets)) {
            list[[i]] <- read_excel(values$file_path, 
                                    sheet = sheets[i])
        }
        
        balneabilidade <<- do.call('rbind', list)
    })
    
    
})

