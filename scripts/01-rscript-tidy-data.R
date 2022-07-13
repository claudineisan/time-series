##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' claudineisan@pastoepixel.com
##################################################################

###
###
#' Pacotes, Funções e Configurações
options(scipen = 9999)

library(tidyverse)
library(readxl)

###
#' importar (input)

#' aquivo csv tendo vírgula como separador decimal e de colunas
# tsd = time serie data frame
tsd <- read_csv('data/landsat_timeserie_1985_2022.csv',
                locale = locale(decimal_mark = ","))
tsd

#' aquivo xlsx tendo ponto como separador decimal
tsd <- read_xlsx('data/landsat_timeserie_1985_2022.xlsx')
tsd

###
#' organizar (tidy)
# pvl_tsd = pivot longer tsd
pvl_tsd <- tsd %>% 
  pivot_longer(!Date, 
               names_to = 'variables',
               values_to = 'values') %>%
  mutate(variables = str_replace(variables, 
                                 '[[:punct:]]', 
                                 '_'),
         ponto = str_replace(
           data.frame(
           do.call('rbind', 
                   str_split(variables, 
                             '_')))[,1],
           'P', ''),
         classe = data.frame(
           do.call('rbind', 
                   str_split(variables, 
                             '_')))[,2],
         banda = data.frame(
           do.call('rbind', 
                   str_split(variables, 
                             '_')))[,3],
         band_ord = as_factor(banda),
         ano = data.frame(
           do.call('rbind', 
                   str_split(Date, 
                             '-')))[,1],
         dia = data.frame(
           do.call('rbind', 
                   str_split(Date, 
                             '-')))[,2],
         banda = ) %>% 
  select(ponto,
         classe,
         banda,
         ano,
         dia,
         values) %>% 
  arrange(ano, 
          dia, 
          banda, 
          ponto, 
          classe)
pvl_tsd

#' arrange the order of bands
pvl_tsd$band_i <- as_factor(pvl_tsd$banda)
levels(pvl_tsd$band_i) <- list(A = 'Coastal', 
                               B = 'Blue',
                               C = 'Green',
                               D = 'Red',
                               E = 'NIR',
                               F = 'SWIR1',
                               G = 'SWIR2')
ord_tsd <- pvl_tsd %>% 
  arrange(band_i) %>% 
  select(-band_i)
ord_tsd

###
#' exportar (output)
write_csv(ord_tsd,
          file = 'cache/tidy_landsat_timeserie_1985_2022.csv')

##################################################################
