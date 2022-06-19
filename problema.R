fluidRow(
  box(width = 6, title = "Distribuci?n de casos por pa?s",
      plotlyOutput("clavegrafico5_a")
  ),
  
  
  box(width = 6, title = "Casos, muertes y vacunaci?n",
      plotlyOutput("clavegrafico6_a")
  )
  
)

