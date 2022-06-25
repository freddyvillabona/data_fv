
# Se cargan las librerias
library("shiny")
library("shinythemes")
library("dplyr")
library("stringr")
library("rticles")
library("ggplot2")
library("formattable")
library("shinydashboard")
library("coronavirus")
library("readr")
library("scales")
library("plotly")

library("pdfetch")
library("xts")

library("spData") 
library("hrbrthemes")



# Dat:
s <- read.csv("emisiones/s.csv")

# ggplot:







# Se carga la data
source("datos.R")
source("mapa_alemania.R")

# Finanzas
lista_f <- c("TSLA","^IXIC","^IBEX","BTC-USD")
#source("funcion.R")



# Se hace la lista despegable
lista <- c("Nuevos_casos","Total_casos", "total_tests", "Pacientes_en_UCI", "Total_muertes", "Total_de_vacunas",
           "Personas_vacunadas","Vacunas_por_cada_cien_HAB",
           "Camas_Hospitalarias_por_cada_mil_HAB",
           "stringency_index"
           
)



#############################

body <- dashboardBody(
  
  ##
  
  
  
  
  ##
  
  tabItems(
    
    ##################
    # Todo el mundo
    ##################  
    tabItem(tabName = "Global",
            
            
            fluidRow(
              box(width = 12, title = "Evolución de casos desde el inicio de la pandemia",
                  plotlyOutput("clavegrafico_t")
              )
            ),
            
            
            fluidRow(
              box(width = 12, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico2_t")
              )
            ),
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico3_t")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico4_t")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico5_t")
              )
              
            )
            
            
            
            
            
    ), # Fin de estructura para "TODO EL MUNDO"
    
    
    
    ##################
    # AFRICA
    ##################  
    tabItem(tabName = "Africa",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_a","Seleccione una opción:", 
                              choices = lista_pais_a, selected = TRUE)
                  
              ),
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_a","Seleccione una opci?n:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_a")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_a")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_a")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_a")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_a")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_a")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_a")
              )
              
            )
            
            
    ),
    
    ##################
    # ASIA
    ##################  
    tabItem(tabName = "Asia",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_as","Seleccione una opción:", 
                              choices = lista_pais_as, selected = TRUE)
                  
                  
              ),
              
              
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_as","Seleccione una opción:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_as")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_as")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_as")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_as")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_as")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_as")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_as")
              )
              
            )
            
            
    ),
    
    
    ##################
    # europa
    ##################  
    tabItem(tabName = "Europa",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_eu","Seleccione una opción:", 
                              choices = lista_pais_eu, selected = TRUE)
                  
                  
              ),
              
              
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_eu","Seleccione una opción:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_eu")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_eu")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_eu")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_eu")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_eu")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_eu")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_eu")
              )
              
            )
            
            
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ##################
    
    
    ##################
    # norte america
    ##################  
    tabItem(tabName = "North_America",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_na","Seleccione una opción:", 
                              choices = lista_pais_na, selected = TRUE)
                  
                  
              ),
              
              
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_na","Seleccione una opción:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_na")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_na")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_na")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_na")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_na")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_na")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_na")
              )
              
            )
            
            
    ),
    
    
    
    
    # sur america
    ##################  
    tabItem(tabName = "South_America",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_sa","Seleccione una opción:", 
                              choices = lista_pais_sa, selected = TRUE)
                  
                  
              ),
              
              
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_sa","Seleccione una opción:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_sa")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_sa")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_sa")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_sa")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_sa")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_sa")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_sa")
              )
              
            )
            
            
    ),
    
    
    ##################
    # oceania
    ##################  
    tabItem(tabName = "Oceania",
            
            fluidRow(
              
              box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavepais_o","Seleccione una opción:", 
                              choices = lista_pais_o, selected = TRUE)
                  
                  
              ),
              
              
              
              box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                  
                  selectInput("clavecoronavirus_o","Seleccione una opción:", 
                              choices = lista, selected = TRUE)
                  
                  
              )),
            
            
            fluidRow(
              box(width = 6, title = "Evolución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico_o")
              ),
              
              
              box(width = 6, title = "Distribución de casos desde el inicio de la pandemia",
                  plotOutput("clavegrafico2_o")
              )
              
            ),
            
            
            fluidRow(
              box(width = 6, title = "Distribución de casos por país",
                  plotlyOutput("clavegrafico3_o")
              ),
              
              
              box(width = 6, title = "Casos, muertes y vacunación",
                  plotlyOutput("clavegrafico4_o")
              )
            ),
            
            
            
            fluidRow(
              
              box(width = 4, title = "Pacientes en UCI",
                  plotlyOutput("clavegrafico5_o")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico6_o")
              ),
              
              box(width = 4, title = "Fumadores",
                  plotlyOutput("clavegrafico7_o")
              )
              
            )
            
            
    ),
    
    
    #########
    ## finanzas
    #########
    
    
    tabItem(tabName = "Yahoo_Finanzas",
            
            
            
            fluidRow(
              
              column(10,
                     plotOutput("clavegrafico1_f")),
              
              column(2,
                     selectInput("opciones",
                                 "Selecciona un Ticket", 
                                 choices = lista_f, 
                                 selected = TRUE),
                     
                     hr(),
                     
                     dateInput("date1", 
                               "Fecha inicio:", 
                               value = "2019-01-01",
                               min = "2015-01-01"),
                     
                     hr(),
                     
                     dateInput("date2", 
                               "Fecha fin:", 
                               value = "2022-05-18",
                               min = "2015-01-01")
                     
                     
              )
              
            ),
            
            fluidRow(
              
              column(10,
                     
                     plotOutput("clavegrafico2_f")
                     
              )
              
            )
            
            
            
            
            
            
            
            
    ),
    
    
    
    tabItem(tabName = "mapa_alemania",
            
            
            fluidRow(
              
              column(12,
                     leafletOutput("clavemapaalemania",
                                   
                                   width = "100%",
                                   height = "800"
                                   
                                   )))
            
    ),
    
    
    
    tabItem(tabName = "plot_emisiones",
            
            
            fluidRow(
              
              column(12,
                     plotOutput("emisiones",
                                
                                width = "100%",
                                height = "800"
                                   
                     )))
            
    ) 
    
    
    
    
    
    
  ) ##### FIN ITEMNS
  
  
  
) ######## FIN BODY





















########
# ui
########
ui <- dashboardPage(
  dashboardHeader(title = "Programando en R"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("COVID-19", tabName = "Global",
               startExpanded = F,
               
               menuSubItem("Todo el mundo", tabName = "Global"),
               menuSubItem("Africa", tabName = "Africa"),
               menuSubItem("Asia", tabName = "Asia"),
               menuSubItem("Europa", tabName = "Europa"),
               menuSubItem("Norte America", tabName = "North_America"),
               menuSubItem("South America", tabName = "South_America"),
               menuSubItem("Oceania", tabName = "Oceania")
               
               
      ),
      
      #menuItem("Mapas", tabName = "map"),
      #menuItem("R Markdown", tabName = "R_Markdown")
      menuItem("Yahoo! Finanzas", tabName = "Yahoo_Finanzas"),
      menuItem("Mapa Leaflet", tabName = "mapa_alemania"),
      menuItem("Emisiones CO2", tabName = "plot_emisiones")
      
      
      
      
      
    )
    
  ),
  body
)


shinyApp(ui = ui, server = function(input, output) {
  
  ###################################
  # GLOBAL 
  ###################################
  
  output$clavegrafico_t <- renderPlotly({
    plot_ly(data= ggg ,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  output$clavegrafico2_t <- renderPlotly({
    plot_ly(data = conf_df_t,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  
  output$clavegrafico3_t <- renderPlotly({ 
    plot_ly(data= ggg ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico4_t <- renderPlotly({ 
    ggplot(ggg, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico5_t <- renderPlotly({ 
    plot_ly(data= ggg ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  ###################################
  # AFRICA 
  ###################################
  
  output$clavegrafico_a <- renderPlot({
    
    Z <- input$clavepais_a
    X <- input$clavecoronavirus_a
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_a <-  renderPlot({
    
    Z <- input$clavepais_a
    X <- input$clavecoronavirus_a
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_a <- renderPlotly({
    plot_ly(data = conf_df_a,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_a <- renderPlotly({
    plot_ly(data= aaa ,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_a <- renderPlotly({ 
    
    Z <- input$clavepais_a
    
    dat_a <- filter(data, location==Z) 
    
    dat_a <- dplyr::select(dat_a, date, Nuevos_casos, new_deaths, 
                           new_vaccinations, female_smokers,
                           male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_a ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_a <- renderPlotly({ 
    
    
    Z <- input$clavepais_a
    
    dat_a2 <- filter(data, location==Z) 
    
    dat_a2 <- dplyr::select(dat_a2, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_a2, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_a <- renderPlotly({ 
    
    
    Z <- input$clavepais_a
    
    dat_a3 <- filter(data, location==Z) 
    
    dat_a3 <- dplyr::select(dat_a3, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_a3 ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###################################
  # Asia
  ###################################
  
  output$clavegrafico_as <- renderPlot({
    
    Z <- input$clavepais_as
    X <- input$clavecoronavirus_as
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_as <-  renderPlot({
    
    Z <- input$clavepais_as
    X <- input$clavecoronavirus_as
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_as <- renderPlotly({
    plot_ly(data = conf_df_as,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_as <- renderPlotly({
    plot_ly(data= aaa_as ,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_as <- renderPlotly({ 
    
    Z <- input$clavepais_as
    
    dat_as <- filter(data, location==Z) 
    
    dat_as <- dplyr::select(dat_as, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_as ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_as <- renderPlotly({ 
    
    
    Z <- input$clavepais_as
    
    dat_a2 <- filter(data, location==Z) 
    
    dat_a2 <- dplyr::select(dat_a2, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_a2, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_as <- renderPlotly({ 
    
    
    Z <- input$clavepais_as
    
    dat_a3 <- filter(data, location==Z) 
    
    dat_a3 <- dplyr::select(dat_a3, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_a3 ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  #####
  #####
  
  
  ###################################
  # europa
  ###################################
  
  output$clavegrafico_eu <- renderPlot({
    
    Z <- input$clavepais_eu
    X <- input$clavecoronavirus_eu
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_eu <-  renderPlot({
    
    Z <- input$clavepais_eu
    X <- input$clavecoronavirus_eu
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_eu <- renderPlotly({
    plot_ly(data = conf_df_eu,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_eu <- renderPlotly({
    plot_ly(data= aaa_eu,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_eu <- renderPlotly({ 
    
    Z <- input$clavepais_eu
    
    dat_eu <- filter(data, location==Z) 
    
    dat_eu <- dplyr::select(dat_eu, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_eu ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_eu <- renderPlotly({ 
    
    
    Z <- input$clavepais_eu
    
    dat_eu <- filter(data, location==Z) 
    
    dat_eu <- dplyr::select(dat_eu, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_eu, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_eu <- renderPlotly({ 
    
    
    Z <- input$clavepais_eu
    
    dat_eu <- filter(data, location==Z) 
    
    dat_eu <- dplyr::select(dat_eu, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_eu ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  ###################################
  # norte america
  ###################################
  
  output$clavegrafico_na <- renderPlot({
    
    Z <- input$clavepais_na
    X <- input$clavecoronavirus_na
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_na <-  renderPlot({
    
    Z <- input$clavepais_na
    X <- input$clavecoronavirus_na
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_na <- renderPlotly({
    plot_ly(data = conf_df_na,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_na <- renderPlotly({
    plot_ly(data= aaa_na ,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_na <- renderPlotly({ 
    
    Z <- input$clavepais_na
    
    dat_na <- filter(data, location==Z) 
    
    dat_na <- dplyr::select(dat_na, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_na ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_na <- renderPlotly({ 
    
    
    Z <- input$clavepais_na
    
    dat_na2 <- filter(data, location==Z) 
    
    dat_na2 <- dplyr::select(dat_na2, date, Nuevos_casos, new_deaths, 
                             new_vaccinations, female_smokers,
                             male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_na2, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_na <- renderPlotly({ 
    
    
    Z <- input$clavepais_na
    
    dat_na3 <- filter(data, location==Z) 
    
    dat_na3 <- dplyr::select(dat_na3, date, Nuevos_casos, new_deaths, 
                             new_vaccinations, female_smokers,
                             male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_na3 ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  
  
  
  
  
  ###################################
  # sur america
  ###################################
  
  output$clavegrafico_sa <- renderPlot({
    
    Z <- input$clavepais_sa
    X <- input$clavecoronavirus_sa
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_sa <-  renderPlot({
    
    Z <- input$clavepais_sa
    X <- input$clavecoronavirus_sa
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_sa <- renderPlotly({
    plot_ly(data = conf_df_sa,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_sa <- renderPlotly({
    plot_ly(data= aaa_sa,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_sa <- renderPlotly({ 
    
    Z <- input$clavepais_sa
    
    dat_sa <- filter(data, location==Z) 
    
    dat_sa <- dplyr::select(dat_sa, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_sa ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_sa <- renderPlotly({ 
    
    
    Z <- input$clavepais_sa
    
    dat_a2 <- filter(data, location==Z) 
    
    dat_a2 <- dplyr::select(dat_a2, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_a2, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_sa <- renderPlotly({ 
    
    
    Z <- input$clavepais_sa
    
    dat_a3 <- filter(data, location==Z) 
    
    dat_a3 <- dplyr::select(dat_a3, date, Nuevos_casos, new_deaths, 
                            new_vaccinations, female_smokers,
                            male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_a3 ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  
  
  
  
  
  ###################################
  # oceanis
  ###################################
  
  output$clavegrafico_o <- renderPlot({
    
    Z <- input$clavepais_o
    X <- input$clavecoronavirus_o
    
    dat <- filter(data, location==Z) 
    
    ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
      ggtitle(X) +
      geom_line(size=.6)  +
      xlab("Fecha") +
      geom_point(size=2.5, shape=21, fill="white", colour ="red") +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
    
  })  
  
  
  output$clavegrafico2_o <-  renderPlot({
    
    Z <- input$clavepais_o
    X <- input$clavecoronavirus_o
    dat <- filter(data, location==Z) 
    
    ggplot(dat, aes(y=new_deaths, x=!!as.name(X))) +
      geom_point() +
      ylab("Muertes") +
      xlab(paste0(X)) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) +
      theme(
        legend.title=element_blank(),
        title =element_text(family="sans",size=18, face='bold', colour = "grey50"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 15,face='bold', 
                                   color = c("grey50")),
        axis.title.y = element_text(size = 16)
      )
  })
  
  
  
  output$clavegrafico3_o <- renderPlotly({
    plot_ly(data = conf_df_o,
            type= "treemap",
            values = ~total_cases,
            labels= ~ country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Casos confirmados",
            textinfo="label+value+percent parent")  
    
  })
  
  
  output$clavegrafico4_o <- renderPlotly({
    plot_ly(data= aaa_o,
            x = ~ fecha,
            y = ~ people_fully_vaccinated ,
            name = 'Total vacunados', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ total_deaths, 
                name = "Total muertos",
                fillcolor = '#E41317') %>%
      add_trace(y = ~total_cases, 
                name = 'Total casos confirmados', 
                fillcolor = 'forestgreen') %>%
      layout(title = "Distribución total de casos",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ###
  output$clavegrafico5_o <- renderPlotly({ 
    
    Z <- input$clavepais_o
    
    dat_o <- filter(data, location==Z) 
    
    dat_o <- dplyr::select(dat_o, date, Nuevos_casos, new_deaths, 
                           new_vaccinations, female_smokers,
                           male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    plot_ly(data= dat_o ,
            x = ~ fecha,
            y = ~ Pacientes_en_UCI ,
            name = 'Pacientes en UCI', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      layout(title = " ",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
  })
  
  
  output$clavegrafico6_o <- renderPlotly({ 
    
    
    Z <- input$clavepais_o
    
    dat_o <- filter(data, location==Z) 
    
    dat_o <- dplyr::select(dat_o, date, Nuevos_casos, new_deaths, 
                           new_vaccinations, female_smokers,
                           male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    ggplot(dat_o, aes(y=female_smokers, x=male_smokers)) +
      geom_point() +
      ylab("Mujeres fumadoras") +
      xlab("Hombres fumadores") +
      ggtitle("Mujeres Vs Hombres") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 4)) +
      scale_x_continuous(labels = comma_format(big.mark = ".",
                                               decimal.mark = ",")) 
    
  })
  
  
  output$clavegrafico7_o <- renderPlotly({ 
    
    
    Z <- input$clavepais_o
    
    dat_o <- filter(data, location==Z) 
    
    dat_o <- dplyr::select(dat_o, date, Nuevos_casos, new_deaths, 
                           new_vaccinations, female_smokers,
                           male_smokers, Pacientes_en_UCI) %>%
      group_by(fecha = lubridate::floor_date(date, "month")) %>%
      summarise(total_cases = sum(Nuevos_casos),
                total_deaths = sum(new_deaths),
                female_smokers = sum(female_smokers),
                male_smokers = sum(male_smokers),
                people_fully_vaccinated = sum(new_vaccinations),
                Pacientes_en_UCI = sum(Pacientes_en_UCI))
    
    
    
    plot_ly(data= dat_o ,
            x = ~ fecha,
            y = ~ female_smokers ,
            name = 'Mujeres fumadoras', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
      add_trace(y = ~ male_smokers, 
                name = "Hombres fumadores",
                fillcolor = 'forestgreen') %>%
      
      layout(title = " ",
             legend = list(x = 0.1, y = 0.5),
             yaxis = list(title = "Número de casos"),
             xaxis = list(title = ""))
    
  })
  
  
  ####
  ####
  
  ###################################
  # GRAFICO 1  
  ###################################
  output$clavegrafico1_f <- renderPlot({
    
    dd1 <- input$date1 
    dd2 <- input$date2 
    op <- input$opciones
    op <- paste0('',op,sep = "")
    
    data <- pdfetch_YAHOO(
      op,
      fields = c("open", 
                 "high", 
                 "low", 
                 "close", 
                 "adjclose", 
                 "volume"),
      from = as.Date(dd1),
      to = as.Date(dd2),
      interval = "1d"
    )
    
    data <- as.data.frame(data)
    data <- tibble::rownames_to_column(data, var = "Fecha")
    data$Fecha <- as.Date(data$Fecha)
    
    names(data) <- c("Fecha","open","high","low","close","adjclose","volume")
    
    library("ggplot2")
    
    ggplot2::ggplot() +
      ggplot2::geom_line(data=data, aes(x=Fecha, y=open, group=1,colour="Open"))+
      ggplot2::geom_line(data=data,aes(x=Fecha, y=high, group=1,colour="High")) +
      ggplot2::geom_line(data=data,aes(x=Fecha, y=low, group=1,colour="Low")) +
      ggplot2::geom_line(data=data,aes(x=Fecha, y=close, group=1,colour="Close")) +
      ggplot2::geom_line(data=data,aes(x=Fecha, y=adjclose, group=1,colour="adjclose")) +
      ggplot2::geom_line(data=data,aes(x=Fecha, y=volume, group=1,colour="Volume")) +
      ggplot2::geom_point() +
      ggplot2::ylab(" ") +
      ggplot2::scale_colour_manual("", 
                                   breaks = c("Open", "High", "Low", "Close","adjclose","Volume"),
                                   values = c("Open"="red", 
                                              "High"="gray", 
                                              "Low"="yellow",
                                              "adjclose"="orange",
                                              "Close"="blue" ,
                                              "Volume"="red")) +
      ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = F)) +
      ggthemes::theme_hc()
    
    
  })
  
  ###################################
  # GRAFICO 2  
  ###################################
  output$clavegrafico2_f <- renderPlot({
    
    dd1 <- input$date1 
    dd2 <- input$date2 
    
    op <- input$opciones
    op <- paste0('',op,sep = "")
    
    data1 <- pdfetch_YAHOO(
      op,
      fields = c("open", 
                 "high", 
                 "low", 
                 "close", 
                 "adjclose", 
                 "volume"),
      from = as.Date(dd1),
      to = as.Date(dd2),
      interval = "1d"
    )
    
    data1 <- as.data.frame(data1)
    data1 <- tibble::rownames_to_column(data1, var = "Fecha")
    data1$Fecha <- as.Date(data1$Fecha)
    
    names(data1) <- c("Fecha","open","high","low","close","adjclose","volume")
    
    
    ggplot2::ggplot() +
      ggplot2::geom_line(data=data1, aes(x=Fecha, y=open, group=1,colour="Open"))+
      ggplot2::geom_line(data=data1,aes(x=Fecha, y=high, group=1,colour="High")) +
      ggplot2::geom_line(data=data1,aes(x=Fecha, y=low, group=1,colour="Low")) +
      ggplot2::geom_line(data=data1,aes(x=Fecha, y=close, group=1,colour="Close")) +
      ggplot2::geom_line(data=data1,aes(x=Fecha, y=adjclose, group=1,colour="adjclose")) +
      ggplot2::geom_point() +
      ggplot2::ylab(" ") +
      
      ggplot2::scale_colour_manual("", 
                                   breaks = c("Open", "High", "Low", "Close","adjclose"),
                                   values = c("Open"="green", 
                                              "High"="gray", 
                                              "Low"="yellow",
                                              "Close"="blue",
                                              "adjclose"="black")) +
      
      
      ggthemes::theme_hc()
    
    
  })
  
  #######
  #######
  
  output$clavemapaalemania <- renderLeaflet({
    
    leaflet::leaflet(station) %>% 
      addTiles() %>%
      addMeasurePathToolbar(options = measurePathOptions(  imperial = F, minPixelDistance = 100, showDistances = T)) %>%
      addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addPolygons(data=adm, weight = 2, fillColor = " ", popup=popup) %>%
      addProviderTiles("Esri", group = "Esri") %>%
      addProviderTiles("Stamen.TonerLite",group = "Toner") %>%
      addLayersControl(baseGroups = c("Esri","Toner"))  %>%
      addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude,
                        labelOptions = labelOptions(noHide = T),
                        icon = color.rojo) %>%
      #addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~k * 1000, popup = ~Station, color = "red")  %>%
      #addCircles(lng = ~stationNairport$Longitude, lat = ~stationNairport$Latitude, weight = 1, radius = ~k * 1000, popup = ~stationNairport$Station, color = "red")  %>%
      #addCircles(lng = ~alternativestation$Longitude, lat = ~alternativestation$Latitude, weight = 1, radius = ~k * 1000, popup = ~alternativestation$Alternative.Station, color = "green")  %>%
      addCircles(lng = ~mainstation$Longitude, lat = ~mainstation$Latitude, weight = 1, radius = ~k3 * 1000, popup = ~map3D$Realistic.Range.with.TOW..km., color = "yellow", fillOpacity = 0.4, labelOptions = labelOptions(noHide = T) )  %>%
      addCircles(lng = ~mainstation$Longitude, lat = ~mainstation$Latitude, weight = 1, radius = ~k2 * 1000, popup = ~map3D$Range.by.the.Manufacturer..km., color = "grey",fillOpacity = 0.7, labelOptions = labelOptions(noHide = T))  %>%
      addCircles(lng = ~mainstation$Longitude, lat = ~mainstation$Latitude, weight = 1, radius = ~k1 * 1000, popup = ~map3D$Recommended.Range.with.TOW..km., color = "blue",  fillOpacity = 0.4, labelOptions = labelOptions(noHide = T))  %>%
      
      
      # addCircles(lng = ~mainstation$Longitude, lat = ~mainstation$Latitude, weight = 1, radius = ~k * 1000, popup = ~Station, color = "lightgoldenrod") 
      addAwesomeMarkers(lng = ~mainstation$Longitude, lat = ~mainstation$Latitude,
                        labelOptions = labelOptions(noHide = T),
                        icon = color.purura)  %>% 
      addAwesomeMarkers(lng = ~alternativestation$Longitude, lat = ~alternativestation$Latitude,
                        labelOptions = labelOptions(noHide = T),
                        icon = color.verde) %>%
      addAwesomeMarkers(lng = ~stationNairport$Longitude, lat = ~stationNairport$Latitude,
                        labelOptions = labelOptions(noHide = T),
                        icon = color.NAirp)  %>%
#      addLegendCustom(opacity = 1, colors = c("white","purple", "red", "green","white","yellow","grey","blue"), 
#                      labels = c("Legend",
#                                 "Mainstation (FRA)", 
#                                 "Vertiport", 
#                                 "Alternate Vertiport",
#                                 "Radius [km]",
#                                 l3,
#                                 l2,
#                                 l1), sizes = c(0,35, 35, 35,0, 35, 35, 35)) %>%
      
      leaflet::addLegend(position = "topright", pal = pal ,values = df$LEGEND,
                         title =  "Radio")
    
    
    
    
    
  })
  
  
  
  ## claveemisiones ##
  output$emisiones <- renderPlot({ 
    
    legend_size <- c(8,9,10,11,12)
    
    
    ggplot(s, aes(gdpPercap, emissions)) +
      ggtitle("Emisiones de CO2, PIB per cápita y población por países - 2020") +
      ylab("Emisiones de CO2 (toneladas métricas por persona)") +
      xlab("PIB per cápita") +
      geom_point(aes(size = pop, 
                     fill = Continente), 
                 shape = 21, 
                 alpha = 0.7) +
      scale_fill_viridis_d(
        guide = guide_legend(override.aes = list(size = 10))) +
      scale_size(range = c(0.1, 30),
                 name="Población (mills.)",
                 breaks = 1000000 * c(250, 500, 750, 1000, 1250),
                 labels = c("280", "560", "840", "1120", "1400"),
      ) +
      geom_text(aes(x = gdpPercap, y = emissions+1.5, label = country),
                color = "blue",
                data = filter(s, pop > 1000000000 | country %in% c("Norway",
                                                                   "Singapore",
                                                                   "Luxembourg",
                                                                   "United States",
                                                                   "Switzerland",
                                                                   "Spain",
                                                                   "Brazil",
                                                                   "Japan",
                                                                   "Qatar",
                                                                   "Kuwait",
                                                                   "Chile",
                                                                   "Netherlands",
                                                                   "Trinidad and Tobago",
                                                                   "United Kingdom"))) +
      theme_ipsum(axis_title_size = 14,
                  base_family = "TT Arial") +
      theme(legend.position="bottom",
            legend.box = "vertical")
    
    
    
    
    
    })
  
  
  

  
  
  ####
  
})

