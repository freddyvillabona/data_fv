
library("coronavirus")
library("dplyr")
library("tidyr")
library("lubridate")

data <- read_rds("data1.rds", refhook = NULL)
data$date <- as.Date(data$date)
data[is.na(data)] <- 0


# Se filtran los datos de chile
#chile <- filter(data, location=="Chile")
#chile$date <- as.Date(chile$date)
#chile[is.na(chile)] <- 0

# se establecen fechas y se renombran las variables
fecha <- format(data$date, "%b-%y")
data <- cbind(data, fecha)

data <- data %>%
  
  rename(Total_casos = total_cases) %>%
  rename(Nuevos_casos = new_cases) %>%
  rename(Total_muertes = total_deaths) %>%
  rename(Total_de_vacunas = total_vaccinations) %>%
  rename(Personas_vacunadas = people_vaccinated) %>%
  rename(Vacunas_por_cada_cien_HAB = total_vaccinations_per_hundred) %>%
  rename(Camas_Hospitalarias_por_cada_mil_HAB = hospital_beds_per_thousand) %>%
  rename(total_tests = total_tests) %>%
  rename(Pacientes_en_UCI = icu_patients)




# AFRICA

lista_pais_a <- filter(data, continent=="Africa")
lista_pais_a <- table(lista_pais_a$location)
lista_pais_a <- as.data.frame(lista_pais_a)
lista_pais_a <- lista_pais_a$Var1

conf_df_a <- coronavirus %>% 
  filter(continent_name=="Africa") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()


aa <- data %>% 
  filter(continent == "Africa") 

names(aa)

aa <- select(aa, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa <- 
  aa %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))




# asiA

conf_df_e <- coronavirus %>% 
  filter(continent_name=="Asia") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()
