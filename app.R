
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

# Se carga la data
#data <- read.csv("owid-covid-data.csv")
source("datos.R")

#data("coronavirus")
#data <- coronavirus


lista_continente <- c("Africa","Asia","Europe","North America",
                      "Oceania","South America")
  

lista_pais <- c("Spain","Brazil","")
  
  
# Se hace la lista despegable
lista <- c("Nuevos_casos","Total_casos", "total_tests", "Pacientes_en_UCI", "Total_muertes", "Total_de_vacunas",
           "Personas_vacunadas","Vacunas_por_cada_cien_HAB",
           "Camas_Hospitalarias_por_cada_mil_HAB")


#############################

body <- dashboardBody(
tabItems(

##################
# Todo el mundo
##################  
    tabItem(tabName = "Global",
          
          fluidRow(
            
            
            box(title = "Seleccionar por continente", " ",width = 4, status = "primary",background = "olive",
                
                selectInput("clavecontinente","Seleccione una opción:", 
                            choices = lista_continente, selected = TRUE)
                
                
            ),
            
            
            
            box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
                
                selectInput("clavepais","Seleccione una opción:", 
                            choices = lista_pais, selected = TRUE)
                
                
            ),
            
            
            
            box(title = "Casos y vacunación", " ",width = 4, status = "primary",background = "olive",
                
                selectInput("clavecoronavirus","Seleccione una opción:", 
                            choices = lista, selected = TRUE)
          
            
          )),
          

          fluidRow(
            box(width = 6, title = "Distribución de casos",
                plotOutput("clavegrafico")
            )
          ),
          
          fluidRow(
            box(width = 12, title = "Gráfico lineal",
                plotOutput("clavegrafico2")
            )
          ),
          
          fluidRow(
            box(width = 12, title = "Resumen estadístico",status = "primary",background = "olive",
                textOutput("texto")
            )
          )
),
#####

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
              
              selectInput("clavecoronavirus_a","Seleccione una opción:", 
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
          
          
          
          
        )
        
        
  
)






########
)
)

# 
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Todo el mundo", tabName = "Global"),
      menuItem("Africa", tabName = "Africa"),
      menuItem("Europa", tabName = "Europe"),
      menuItem("Norte Ámerica", tabName = "North_America"),
      menuItem("Sur Ámerica", tabName = "South_America")
      
    
      
      
    )
    
  ),
  body
)


shinyApp(ui = ui, server = function(input, output) {

###################################
# GLOBAL 
###################################
output$clavegrafico <- renderPlot({

  Z <- input$clavepais
  Y <- input$clavecontinente
  X <- input$clavecoronavirus
  
  dat <- filter(data, continent==Y) %>%
    filter(location==Z) 
  
  ggplot2::ggplot(data=dat, aes(x=date, y=!!as.name(X))) + 
    ggtitle(X) +
    geom_line(size=.8)  +
    geom_point( size=3, shape=21, fill="white", colour ="red") +
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





###################################
# GRAFICO 2  
###################################

  

###############################################

#output$texto <- renderText({ 
  
#  z <- select(chile, input$clavecoronavirus)
#  summary(z)
  
#  })

})


