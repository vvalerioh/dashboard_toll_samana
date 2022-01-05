library(tidyverse)
library(purrr)

# Load data ----
df.cost <- read.csv('data/toll_cost_2021_2022.csv') %>%
  rename(Estacion = 1)
# Write function ----
calc_toll <- function(origin = 'Marbella',
                      destination = 'Marbella',
                      type = 'ida',
                      categoria = 'Categoria 1'){
  estaciones <- c('Marbella', 'Naranjal', 'Guaraguao', 'Catey')
  or.code <- case_when(
    origin == 'Marbella' ~ 1,
    origin == 'Naranjal' ~ 2,
    origin == 'Guaraguao' ~ 3,
    origin == 'Catey' ~ 4)
  dest.code <- case_when(
    destination == 'Marbella' ~ 1,
    destination == 'Naranjal' ~ 2,
    destination == 'Guaraguao' ~ 3,
    destination == 'Catey' ~ 4)
  cost <- 0
  old.cost <- 0
  for (i in or.code:dest.code){
    cost <- cost + df.cost %>% filter(Estacion == estaciones[i],
                                      Categoria == categoria) %>%
      select(Tarifa_Nueva)
    old.cost <- old.cost + df.cost %>% filter(Estacion == estaciones[i],
                                              Categoria == categoria) %>%
      select(Tarifa_Anterior)
  }
  if(type == 'ida y vuelta'){
   cost <- cost * 2
   old.cost <- old.cost * 2
  }
  res <- paste0('El nuevo costo de peaje de ', type,
                ' para un vehículo ' ,
                categoria, ', ',
               'desde el peaje de ',
               origin,
               ' hasta el peaje de ',
               destination,
               ' es de ',
               cost, ' pesos. ',
               case_when(cost < old.cost ~ paste0('Esto representa una disminucion de ',
                                                  old.cost-cost,
                                                ' pesos.'),
                         old.cost > cost ~ paste0('Esto representa un aumento de ',
                                                   cost - old.cost,
                                                   ' pesos.'),
                         old.cost == cost ~ paste0('Esta tarifa no presenta cambio.') ))
  return(res)
  
}

# # Test function
# calc_toll(origin = 'Marbella', destination = 'Catey',
#           type = 'ida y vuelta',
#           categoria = 'Categoria 4')
