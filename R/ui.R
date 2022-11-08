title <- tags$header(tags$img(src="sigma.png", height = '40', width = '38',
),"Coliformes")

ui <- fluidPage(
    
    dashboardPage(
        dashboardHeader(title = title),
        # Side Bar ####
        dashboardSidebar(
          collapsed = T,
          fileInput('file1', 'Escolha o seu arquivo',
                    accept = c(".xls")),
          numericInput("ai", "Ano inicial",
                       value = 2010),
          numericInput("af", "Valor final",
                       value = 2019)
        ),
        # Body ####
        dashboardBody(
            tags$head(
                tags$style(HTML(
                    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    ')),
                
                tags$link(rel = "stylesheet", type = "text/css", href = "customc.css")),
            tags$script(HTML('
        $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Gráfico de controle e indicadores de capacidade do processo para as praias de Natal </span>\');
        })
        
        ')),
            
            
            tabItem(tabName = "desem",
                    tabBox(
                        # The id lets us use input$tabset1 on the server to find the current tab
                        height = "540px", width = 12,
                        tabPanel("Gráfico de controle",
                                 fluidRow(width=12,
                                          column(width=2,
                                                 selectInput("ah", "Escolha por Ano",
                                                             choices = unique(dff$Ano), 
                                                             selected = 2019, multiple = TRUE,
                                                             selectize=T, width = '98%')),
                                          column(width=2,
                                                 selectInput("bh", "Praias",
                                                             choices = unique(dff$Praia), 
                                                             selected = unique(dff$Praia)[1], multiple = FALSE,
                                                             selectize=F, width = '98%')),
                                          column(width=3,
                                                 selectInput("ch", "Ver os pontos fora de controle?",
                                                             choices = c("Não","Sim"), 
                                                             selected = "Sim", multiple = FALSE,
                                                             selectize=F, width = '98%')),
                                          column(width=5,
                                                 uiOutput("info"))),
                                 fluidRow(
                                   column(width=12,
                                          plotlyOutput("plot1")))
        ),
        tabPanel("Índice de capacidade",
                 fluidRow(width=12,
                          column(width=3,
                                 selectInput("ah1", "Escolha por Ano",
                                             choices = unique(dff$Ano), 
                                             selected = c(2019,2018,2017,2016), multiple = TRUE,
                                             selectize=T, width = '98%')),
                          column(width=3,
                                 selectInput("bh1", "Praias",
                                             choices = unique(dff$Praia), 
                                             selected = unique(dff$Praia)[1], multiple = TRUE,
                                             selectize=T, width = '98%')),
                          column(width=3,
                                 numericInput("dh", "Valor alvo",
                                             value = 250))),
                 fluidRow(
                   column(width=6,
                          plotlyOutput("plot2")),
                   column(width=6,
                          dataTableOutput("dt1"))
                 )
        )
            
        )
    )
)))