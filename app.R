
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
        
        
)


    
      
          
          
          

) ##### FIN ITEMNS



) ######## FIN BODY


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
  
  dat_a <- select(dat_a, date, Nuevos_casos, new_deaths, 
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
  
  dat_a2 <- select(dat_a2, date, Nuevos_casos, new_deaths, 
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
  
  dat_a3 <- select(dat_a3, date, Nuevos_casos, new_deaths, 
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

###

})

