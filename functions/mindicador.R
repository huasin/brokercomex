mindicador <- function(currency, date){
  library(magrittr)
  # currency = 'dolar'; date = '2020'
  url <- stringr::str_glue("https://mindicador.cl/api/{currency}/{date}")
  get <- httr::GET(url)
  df <- jsonlite::fromJSON(httr::content(get,'text'), simplifyVector = T) %>% 
    .$serie %>% 
    dplyr::mutate(fecha = lubridate::ymd_hms(fecha) %>% as.Date,
                  mes = lubridate::floor_date(fecha, "months")) %>% 
    dplyr::group_by(mes) %>% 
    dplyr::filter(fecha == max(fecha)) %>% 
    ungroup %>% 
    select(-fecha)
  return(df)
}
