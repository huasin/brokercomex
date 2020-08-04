rm(list = ls())
source('init.R')
source('functions/mindicador.R')
library(tidyverse)

param <- list()
param$mes <- as.Date('2020-06-01')

# data --------------------------------------------------------------------

prod <- readxl::read_xls('files/202006_produccion_reaseguro.xls', .name_repair = janitor::make_clean_names)
str(prod)

# Manipulacion ------------------------------------------------------------

dolar <- mindicador('dolar',2020)
dolar_mes <- dolar %>% filter(mes == !!param$mes) %>% pull(valor)
ri <- 0.0009

prod <- prod %>% 
  mutate(poliza_num = poliza %>% str_remove_all("-") %>% as.numeric,
         fecha_solicitud = lubridate::dmy(fecha_solicitud),
         fecha_de_arribo = fecha_de_arribo %>% as.numeric %>% as.Date(origin = '1899-12-30'),
         fecha_embarque = lubridate::dmy(fecha_embarque),
         x = purrr::map_dbl(monto_asegurado_us, ~max(.x*!!ri,10)),
         monto_asegurado_clp = monto_asegurado_us*!!dolar_mes,
         prima_minima_clp = prima_minima*!!dolar_mes,
         prima_ri_clp = prima_ri*!!dolar_mes)

clientes <- unique(prod$cliente)

resumen <- prod %>% 
  group_by(cobertura = "FORBES", cliente) %>% 
  summarise(factura = sum(prima_ri_clp, na.rm = T))


get_data_cliente <- purrr::as_mapper(~prod %>% filter(cliente == !!.x))

prod_cliente <- purrr::map(clientes, get_data_cliente) %>% 
  set_names(clientes)

prod_cliente$resumen <- resumen

openxlsx::write.xlsx(prod_cliente, 'files/output.xlsx')


# exploratorio ------------------------------------------------------------

head(prod)