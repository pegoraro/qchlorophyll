################################################################################
#             SCRIPT CALCOLO STATISTICHE DESCRITTIVE QCHLOROPHYLL
################################################################################
# Note:
# Funziona con qchlorophyll versione 0.2


# Inizio script di esempio
################################################################################
# Set up

# Installo e carico la libreria qchlorophyll versione 0.2
install.packages("/home/qchlorophyll_0.2.tar.gz", repos = NULL)
require(qchlorophyll)

################################################################################
# Caricamento files di test (identico al primo script)

# Path
nc_files_path <- "/home/data_nc"
# Carico file .nc ed estraggo CHL1_mean
nc_files_list <- load_all_as_list(path = nc_files_path, variables = c("CHL1_mean"))
# Unisco il tutto in un unico dataframe.
nc_dataframe <- assign_id_and_melt(nc_files_list)
# I dati caricati pronti per le analisi
View(nc_dataframe)

################################################################################
# Calcolo statistiche descrittive

# Calcolo media, deviazione standard, dati mancanti e numerosità campionaria.
#
# Nota1: di default la variabile sulla quale sono calcolate le statistiche è "CHL1_mean".
# Nel caso in cui cambiasse, è sufficiente cambiare il parametro "variable".
#
# Nota2: L'output di questa funzione è il dataframe nc_dataframe con aggiunte
# le variabili "avg", "sdv", "NAs_count", "n_count".
#
media_e_sd <- aggregate_statistics(nc_dataframe,
                                   stat_funs = list(avg = "mean(., na.rm=TRUE)",
                                                    sdv ="sd(., na.rm=TRUE)",
                                                    NAs_count = "sum(is.na(.))",
                                                    n_count = "n()"),
                                   variable = "CHL1_mean")

################################################################################
# Reshape delle statistiche calcolate.

# Calcolate le statistiche, vogliamo passare da un dataframe di tipo long
# lon lat id_pixel media id_date ...
# xx  yy  1        m1    d1
# xx  yy  1        m2    d2
#
# A un dataframe di tipo wide per ogni variabile (media, deviazione standard, dati mancanti e numerosità campionaria).
# Ogni riga rappresenta un pixel, ogni colonna rappresenta un preciso giorno giuliano. Nella "griglia" avremo i valori
# della statistica calcolata al punto precedente per ogni combinazione di pixel/giorno giuliano.
# lon lat id_pixel d_001 d_002 ...
# xx  yy  1        v11   v12   ...
# xz  yz  2        v21   v22   ...
#
# Questa funzione ha come output una lista con tanti elementi quante sono le statistiche calcolate al punto precedente,
# dove ogni elemento della lista è un dataframe in formato wide.
#
# Nota1: La funzione trova automaticamente il nome delle statistiche calcolate e fa il reshape su tutte.
#
media_e_sd_reshaped <- reshape_statistics(media_e_sd)

################################################################################
# Filtraggio dati mancanti

# Filtro fuori dati con piu di due NA. NA ridotti al 12.5%. 25723 su 5520*37
stats_reshaped_less_NA <- filter_out_na(media_e_sd, media_e_sd_reshaped, max_missing_periods = 2, unique_id = "id_pixel")

# Fine script esempio
################################################################################
