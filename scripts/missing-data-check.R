################################################################################
# Analisi veloce dati mancanti
require(qchlorophyll)
require(dplyr)

# Carico dati
path <- "~/home/data"
dati <- load_all_as_list(path = path)
dati <- assign_id_and_melt(dati)

# Calcolo statistiche aggregate: media e numero di osservazioni mancanti.
e_1 <- aggregate_statistics(dati, stat_funs = list(avg = "mean(., na.rm=TRUE)", NAs_count = "sum(is.na(.))"))
# Fai il reshape
e_1_1 <- reshape_statistics(e_1)
# Togli gli NA dalla lista con le statistiche
no_na <- filter_out_na(e_1, e_1_1, max_missing_periods = 2, unique_id = "id_pixel")

# Pixel totali di partenza = 5520

# Verifichiamo quanti pixel verrebbero rimossi, utilizzando il dataframe con le statistiche
e_1 %>% group_by(id_pixel) %>% filter(NAs_count <= 2) %>% select(id_pixel) %>% distinct() %>% nrow()
# Ritorna 3718 righe ossia 3718 pixel.
# 3718/5520 = 0.673. Si tiene circa il 67% dei pixel.

# Controprova dopo il filtraggio: seleziona i pixel unici rimasti
no_na[[1]] %>% select(id_pixel) %>% distinct() %>% nrow()
# Ritorna 3718. Ok.
