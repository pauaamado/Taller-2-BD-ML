

library(skimr)
library(stargazer)
library(egg)
# Estadísticas descrpitivas -----------------------------------------------

df_personas_total <- rbind(df_personas_test %>% select(id,Orden,  P6040, P6210,  P6050, P6050, P6920, P6020,  Oc, Pet), 
                           df_personas_train %>%  select(id, Orden, P6040, P6210, P6050, P6050, P6920, P6020, Oc, Pet))
df_hogares_total <- rbind(df_hogares_test %>% select(id, Nper, P5010, P5090), df_hogares_train %>% select(id, Nper, P5010, P5090)) 

#Maximo grado educativo
df_personas_train_educ <- df_personas_total %>%
  group_by(id) %>% 
  filter(P6040 >= 10) %>%  #filtrar los niños
  reframe(maximo_grado_educ = as.factor(max(P6210, na.rm = TRUE))) 


#Mujeres madres de hojar
df_personas_train_mujeres_cab <- df_personas_total %>%
  group_by(id) %>% 
  mutate(hay_esposo = ifelse(P6050 == 2, 1, 0),
         esposo = cumsum(hay_esposo), 
         n_personas = cumsum(Orden), 
         mujer_soltera = ifelse(P6020 == 2 & P6050 == 1 & esposo == 0 & n_personas > 1, 1, 0)) %>% 
  filter(P6050 == 1) %>% 
  select(id, mujer_soltera)

#Ocupados
df_personas_train_ocup <- df_personas_total %>%
  mutate(cotiza = case_when(P6920 == 1 | P6920 == 3 ~ 1, TRUE ~ 0)) %>% 
  group_by(id) %>% 
  filter(Pet == 1) %>%  #filtrar los niños
  reframe(ocupados = sum(Oc, na.rm = TRUE), 
          pet = sum(Pet, na.rm = TRUE), 
          proporcion_ocupados = ocupados/pet,
          cotizantes = sum(cotiza), 
          proporcion_cotizantes = cotizantes/pet) %>% 
  mutate(proporcion_cotizantes = ifelse(proporcion_cotizantes > 1, 1, proporcion_cotizantes)) 


#Número de niños en el hogar
df_personas_train_niños <- df_personas_total %>%
  group_by(id) %>% 
  filter(P6040 <= 13) %>%  #filtrar los niños
  reframe(num_niños = n())


df_variables_total <- df_hogares_total %>% 
  left_join(df_personas_train_educ, by = "id") %>% 
  left_join(df_personas_train_ocup) %>%  
  left_join(df_personas_train_mujeres_cab) %>% 
  left_join(df_personas_train_niños) %>% 
  mutate(hacinamiento = Nper/P5010, 
         tipo_vivienda = as.factor(P5090), 
         num_niños = ifelse(is.na(num_niños), 0, num_niños), 
         proporcion_cotizantes = ifelse(is.na(proporcion_cotizantes), 0, proporcion_cotizantes),
         niños_madre = num_niños*mujer_soltera) %>% 
  select(maximo_grado_educ, hacinamiento, proporcion_cotizantes,  num_niños, mujer_soltera, tipo_vivienda) 


df_skimr <-  df_variables_total %>% 
  skimr::skim() %>% 
  as.data.frame() %>% 
  select(-c(n_missing, complete_rate, factor.ordered, factor.n_unique))


stargazer::stargazer(df_skimr)



df_personas_train_educ <- df_personas_train %>%
  group_by(id) %>% 
  filter(P6040 >= 10) %>%  #filtrar los niños
  reframe(maximo_grado_educ = as.factor(max(P6210, na.rm = TRUE))) 


#Mujeres madres de hojar
df_personas_train_mujeres_cab <- df_personas_train %>%
  group_by(id) %>% 
  mutate(hay_esposo = ifelse(P6050 == 2, 1, 0),
         esposo = cumsum(hay_esposo), 
         n_personas = cumsum(Orden), 
         mujer_soltera = ifelse(P6020 == 2 & P6050 == 1 & esposo == 0 & n_personas > 1, 1, 0)) %>% 
  filter(P6050 == 1) %>% 
  select(id, mujer_soltera)

#Ocupados
df_personas_train_ocup <- df_personas_train %>%
  mutate(cotiza = case_when(P6920 == 1 | P6920 == 3 ~ 1, TRUE ~ 0)) %>% 
  group_by(id) %>% 
  filter(Pet == 1) %>%  #filtrar los niños
  reframe(ocupados = sum(Oc, na.rm = TRUE), 
          pet = sum(Pet, na.rm = TRUE), 
          proporcion_ocupados = ocupados/pet,
          cotizantes = sum(cotiza), 
          proporcion_cotizantes = cotizantes/pet) %>% 
  mutate(proporcion_cotizantes = ifelse(proporcion_cotizantes > 1, 1, proporcion_cotizantes)) 


#Número de niños en el hogar
df_personas_train_niños <- df_personas_train %>%
  group_by(id) %>% 
  filter(P6040 <= 13) %>%  #filtrar los niños
  reframe(num_niños = n())


#Hacinamiento y tipo_vivienda
df_hogares_train_variables <- df_hogares_train %>% 
  left_join(df_personas_train_educ, by = "id") %>% 
  left_join(df_personas_train_ocup) %>%  
  left_join(df_personas_train_mujeres_cab) %>% 
  left_join(df_personas_train_niños) %>% 
  mutate(hacinamiento = Nper/P5010, 
         tipo_vivienda = as.factor(P5090), 
         num_niños = ifelse(is.na(num_niños), 0, num_niños), 
         proporcion_cotizantes = ifelse(is.na(proporcion_cotizantes), 0, proporcion_cotizantes),
         niños_madre = num_niños*mujer_soltera) %>% 
  select(Pobre, maximo_grado_educ, hacinamiento, proporcion_cotizantes,  num_niños, mujer_soltera, tipo_vivienda) %>% 
  mutate( Maximo_nivel_educativo_hogar = case_when(
    maximo_grado_educ == 1 ~ '0. Ninguno',
    maximo_grado_educ == 2 ~ '1. Preescolar',
    maximo_grado_educ == 3 ~ '2. Primaria',
    maximo_grado_educ == 4 ~ '3. Secundaria',
    maximo_grado_educ == 5 ~ '4. Media',
    maximo_grado_educ == 6 ~ '5. Superior', 
    maximo_grado_educ == 9 ~ '2. Primaria'), 
    tipo_vivienda_descripcion = case_when(
      tipo_vivienda == 1 ~ "1. Propia, pagada",
      tipo_vivienda == 2 ~ "2. Propia, la están pagando",
      tipo_vivienda == 3 ~ "3. En arriendo",
      tipo_vivienda == 4 ~ "4. En usufructo",
      tipo_vivienda == 5 ~ "5. Ocupante",
      tipo_vivienda == 6 ~ "6. Otra",
      TRUE ~ NA_character_  # Para manejar cualquier otro valor fuera del rango
    ))      




grado_educ <- ggplot(df_hogares_train_variables, aes(x = Maximo_nivel_educativo_hogar, fill = factor(Pobre))) +
  geom_histogram(stat = "count", position = "dodge") +
  labs(title = "Distribución de máximo grado educativo por condición de pobreza",
       x = "Máximo grado educativo",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  



hacinamiento <- ggplot(df_hogares_train_variables, aes(x = hacinamiento, fill = factor(Pobre))) +
  geom_histogram(position = "dodge") +
  labs(title = "Distribución del hacinamiento por condición de pobreza",
       x = "Hacinamiento",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  



proporcion_cotizantes <- ggplot(df_hogares_train_variables, aes(x = proporcion_cotizantes, fill = factor(Pobre))) +
  geom_histogram(position = "dodge") +
  labs(title = "Distribución de la proporción de cotizantes por condición de pobreza",
       x = "Proporcion cotizantes",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  


num_niños <- ggplot(df_hogares_train_variables, aes(x = num_niños, fill = factor(Pobre))) +
  geom_histogram(position = "dodge") +
  labs(title = "Distribución de la proporción de número de niños por condición de pobreza",
       x = "Número de niños",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  



mujer_soltera <- ggplot(df_hogares_train_variables, aes(x = mujer_soltera, fill = factor(Pobre))) +
  geom_histogram(position = "dodge") +
  labs(title = "Distribución de hogares en cabeza de mujeres por condición de pobreza",
       x = "Hogar a cargo de una mujer soltera",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  



tipo_vivienda <-  ggplot(df_hogares_train_variables, aes(x = tipo_vivienda_descripcion, fill = factor(Pobre))) +
  geom_histogram(position = "dodge", stat = "count") +
  labs(title = "Distribución de la proporción de tipo de vivienda por condición de pobreza",
       x = "Tipo de vivienda",
       y = "Conteo",
       fill = "Pobre (0 = No, 1 = Sí)") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "lightskyblue")) + 
  theme(panel.grid = element_blank())  



ggarrange(grado_educ, hacinamiento, proporcion_cotizantes, num_niños, mujer_soltera, tipo_vivienda)
