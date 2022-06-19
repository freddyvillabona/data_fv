
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


conf_df_t <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()


# Global

gg <- select(data, date, Nuevos_casos, 
             new_deaths, new_vaccinations,
             female_smokers, male_smokers, 
             Pacientes_en_UCI)
ggg <- 
  gg %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            female_smokers = sum(female_smokers),
            male_smokers = sum(male_smokers),
            people_fully_vaccinated = sum(new_vaccinations),
            Pacientes_en_UCI = sum(Pacientes_en_UCI))

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

aa <- select(aa, date, Nuevos_casos, new_deaths, 
             new_vaccinations, female_smokers,
             male_smokers, Pacientes_en_UCI)
aaa <- 
  aa %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            female_smokers = sum(female_smokers),
            male_smokers = sum(male_smokers),
            people_fully_vaccinated = sum(new_vaccinations),
            Pacientes_en_UCI = sum(Pacientes_en_UCI))


# asiA

lista_pais_as <- filter(data, continent=="Asia")
lista_pais_as <- table(lista_pais_as$location)
lista_pais_as <- as.data.frame(lista_pais_as)
lista_pais_as <- lista_pais_as$Var1

conf_df_as <- coronavirus %>% 
  filter(continent_name=="Asia") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

aa_as <- data %>% 
  filter(continent == "Asia") 

aa_as <- select(aa_as, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa_as <- 
  aa_as %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))



# europa

lista_pais_eu <- filter(data, continent=="Europe")
lista_pais_eu <- table(lista_pais_eu$location)
lista_pais_eu <- as.data.frame(lista_pais_eu)
lista_pais_eu <- lista_pais_eu$Var1

conf_df_eu <- coronavirus %>% 
  filter(continent_name=="Europe") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

aa_eu <- data %>% 
  filter(continent == "Europe") 

aa_eu <- select(aa_eu, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa_eu <- 
  aa_eu %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))


# norte america

lista_pais_na <- filter(data, continent=="North America")
lista_pais_na <- table(lista_pais_na$location)
lista_pais_na <- as.data.frame(lista_pais_na)
lista_pais_na <- lista_pais_na$Var1

conf_df_na <- coronavirus %>% 
  filter(continent_name=="North America") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

aa_na <- data %>% 
  filter(continent == "North America") 

aa_na <- select(aa_eu, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa_na <- 
  aa_na %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))





# sur america

lista_pais_sa <- filter(data, continent=="South America")
lista_pais_sa <- table(lista_pais_sa$location)
lista_pais_sa <- as.data.frame(lista_pais_sa)
lista_pais_sa <- lista_pais_sa$Var1

conf_df_sa <- coronavirus %>% 
  filter(continent_name=="South America") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

aa_sa <- data %>% 
  filter(continent == "South America") 

aa_sa <- select(aa_sa, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa_sa <- 
  aa_sa %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))





# oceania

lista_pais_o <- filter(data, continent=="Oceania")
lista_pais_o <- table(lista_pais_o$location)
lista_pais_o <- as.data.frame(lista_pais_o)
lista_pais_o <- lista_pais_o$Var1

conf_df_o <- coronavirus %>% 
  filter(continent_name=="Oceania") %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

aa_o <- data %>% 
  filter(continent == "Oceania") 

aa_o <- select(aa_o, date, Nuevos_casos, new_deaths, new_vaccinations)
aaa_o <- 
  aa_o %>%
  group_by(fecha = lubridate::floor_date(date, "month")) %>%
  summarise(total_cases = sum(Nuevos_casos),
            total_deaths = sum(new_deaths),
            people_fully_vaccinated = sum(new_vaccinations))



