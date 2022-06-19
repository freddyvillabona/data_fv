##################
# AFRICA
##################  
tabItem(tabName = "Africa",
        
        fluidRow(
          
          box(title = "Seleccionar por país", " ",width = 4, status = "primary",background = "olive",
              
              selectInput("clavepais_a","Seleccione una opción:", 
                          choices = lista_pais_a, selected = TRUE)
              
              
          ),
          
          
          
          box(title = "Casos y vacunaci?n", " ",width = 4, status = "primary",background = "olive",
              
              selectInput("clavecoronavirus_a","Seleccione una opci?n:", 
                          choices = lista, selected = TRUE)
              
              
          )),
        
        
        fluidRow(
          box(width = 6, title = "Evolucion de casos desde el inicio de la pandemia",
              plotOutput("clavegrafico_a")
          ),
          
          
          box(width = 6, title = "Distribuci?n de casos desde el inicio de la pandemia",
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
        
        
        
        
        
        
        
        
        