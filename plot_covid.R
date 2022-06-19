library(plotly)
library(tidyr)
library(tidyr)
library(lubridate)
library(readr)
library(coronavirus)

data <- read_rds("data1.rds", refhook = NULL)
data$date <- as.Date(data$date)
data[is.na(data)] <- 0


aa <- data %>% 
  filter(continent == "Africa") 

aa <- select(aa,date, new_cases, new_deaths, new_vaccinations)
aaa <- 
  aa %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(new_cases),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))


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

  
data(coronavirus)
data <- coronavirus
cv <- data %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovery) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovery),
         death_total = cumsum(death)) 


  plot_ly(data= cv ,
          x = ~ date,
          y = ~ active_total,
          name = 'Activos', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Muertos",
            fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recuperados', 
            fillcolor = 'forestgreen') %>%
  layout(title = "Distribución de casos",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Número de casos"),
         xaxis = list(title = ""))


  
  
  library(plotly)
  
  
  conf_df <- coronavirus %>% 
    filter(continent_name=="Africa") %>%
    filter(type == "confirmed") %>%
    group_by(country) %>%
    summarise(total_cases = sum(cases)) %>%
    arrange(-total_cases) %>%
    mutate(parents = "Confirmed") %>%
    ungroup() 
  
  
  
  plot_ly(data = conf_df,
          type= "treemap",
          values = ~total_cases,
          labels= ~ country,
          parents=  ~parents,
          domain = list(column=0),
          name = "Casos confirmados",
          textinfo="label+value+percent parent")  
  
  
  
######
#  FUMADORES
######  
  
  fumadores <- data %>% 
    filter(continent == "Africa") 
  
  fumadores <- select(fumadores, date, 
                      male_smokers,
                      female_smokers
                      )
  fumadores <- 
    fumadores %>%
    group_by(fecha = lubridate::floor_date(date, "month")) %>%
    summarise(female_smokers = sum(female_smokers),
              male_smokers = sum(male_smokers))  
  
  
  
  
  plot_ly(data= fumadores ,
          x = ~ fecha,
          y = ~ female_smokers,
          name = 'Activos', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
    add_trace(y = ~ male_smokers, 
              name = "male_smokers",
              fillcolor = '#E41317') %>%
#    add_trace(y = ~recovered_total, 
#              name = 'Recuperados', 
#              fillcolor = 'forestgreen') %>%
    layout(title = "Distribución de casos",
           legend = list(x = 0.1, y = 0.9),
           yaxis = list(title = "Número de casos"),
           xaxis = list(title = "Datos: Johns Hopkins University Center for Systems Science and Engineering"))
  
  names(fumadores)
  
  
  p3 <- plot_ly(fumadores,
                x = ~fecha,
                y = ~male_smokers,
                type = "bar",
                name = "Hombres") %>% 
    add_trace(y = ~female_smokers,
              name = "Mujeres") %>% 
    layout(yaxis = list(title = "Número de contagios"),
           barmode = "group")
  p3
  
  
  p4 <- plot_ly(gBarChart,
                x = ~Cities,
                y = ~GroupA,
                type = "bar",
                name = "group A") %>% 
    add_trace(y = ~GroupB,
              name = "Group B") %>% 
    layout(yaxis = list(title = "Cities"),
           barmode = "stack")
  p4  
  