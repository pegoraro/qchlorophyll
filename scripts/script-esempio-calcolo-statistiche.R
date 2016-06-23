################################################################################
#             SCRIPT CALCOLO STATISTICHE DESCRITTIVE QCHLOROPHYLL
################################################################################
# Note:
# Funziona con qchlorophyll versione 0.2


# Inizio script di esempio
################################################################################
# Set up

# Installo e carico la libreria qchlorophyll versione 0.2. (sostituire path e nome del .tar.gz)
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

# Calcolo media, deviazione standard, dati mancanti, numerosità campionaria e coefficiente
# di variazione (per ogni gruppo).
#
# Nota1: di default la variabile sulla quale sono calcolate le statistiche è "CHL1_mean".
# Nel caso in cui cambiasse, è sufficiente cambiare il parametro "variable".
#
# Nota2: L'output di questa funzione è il dataframe nc_dataframe con aggiunte
# le variabili "avg", "sdv", "NAs_count", "n_count", "coeff_var".
#
# Nota3: il coefficiente di variazione (deviazione standard / v. assoluto della media) può
# essere calcolato con due sintassi diverse, in modo coerente rispetto a come sono state
# specificate le altre funzioni per il calcolo delle altre statistiche è possibile usare
# le due funzioni mean e sd, in alternativa è possibile utilizzare il rapporto tra le
# colonne già calcolate (opzione commentata).
#
# Infine siccome la media è sempre positiva non ho usato il valore assoluto.
#
#
# Nota aggiuntiva:
# La funzione aggregate_statistics è fatta in modo che tu possa passarle tramite la lista
# stat_funs una lista di funzioni arbitrarie che accettano come input un vettore e ritornano
# un numero. Ad esempio, potresti definirti una tua funzione che calcola una particolare
# statistica e passarla a aggregate_statistics. Facciamo un esempio con il coefficiente
# di variazione:
#
# coeff_var <- function(x){ sd(x, na.rm = TRUE) / mean(x,na.rm = TRUE)}
#
# media_e_sd1 <- aggregate_statistics(nc_dataframe,
#                                     stat_funs = list(coeff_variazione = "coeff_var(.)"),
#                                     variable = "CHL1_mean")
#
media_e_sd1 <- aggregate_statistics(nc_dataframe,
                                   stat_funs = list(avg = "mean(., na.rm=TRUE)",
                                                    sdv ="sd(., na.rm=TRUE)",
                                                    NAs_count = "sum(is.na(.))",
                                                    n_count = "n()",
                                                    #coeff_var = "sdv/avg"),
                                                    coeff_var = "sd(.,na.rm=TRUE)/mean(., na.rm=TRUE)"),
                                   variable = "CHL1_mean")

# Per maggiori informazioni, controllare l'help della funzione
?aggregate_statistics
# Oppure
?qchlorophyll::aggregate_statistics

################################################################################
# Reshape delle statistiche calcolate.

# Calcolate le statistiche, vogliamo passare da un dataframe di tipo long
# lon lat id_pixel media id_date ...
# xx  yy  1        m1    d1
# xx  yy  1        m2    d2
#
# a un dataframe di tipo wide per ogni variabile (media, deviazione standard, dati mancanti e numerosità campionaria).
# Ogni riga rappresenta un pixel, ogni colonna rappresenta un preciso giorno giuliano. Nella "griglia" avremo i valori
# della statistica calcolata al punto precedente per ogni combinazione di pixel/giorno giuliano.
# lon lat id_pixel d_001 d_002 ...
# xx  yy  1        v11   v12   ...
# xz  yz  2        v21   v22   ...
#
# Questa funzione ha come output una lista con tanti elementi quante sono le statistiche calcolate al punto precedente,
# e ogni elemento della lista è un dataframe in formato wide.
#
# Nota1: La funzione trova automaticamente il nome delle statistiche calcolate e fa il reshape su tutte.
#
media_e_sd_reshaped <- reshape_statistics(media_e_sd)

# Per maggiori informazioni, controllare l'help della funzione
?reshape_statistics
# Oppure
?qchlorophyll::reshape_statistics

################################################################################
# Filtraggio dati mancanti

# L'output di questa funzione è una lista di dataframe (come quella di reshape_statistics)
# senza i pixel rimossi.

# Il filtro viene applicato sulla serie temporale TS come da indicazioni nell'email
# Il numero di pixel rimanenti viene stampato dalla funzione a video.

media_e_sd_reshaped_less_NA <- filter_out_na(reshaped_data_list = media_e_sd_reshaped,
                                             max_missing_periods =  2)

# Nota3: Vengono tenuti tutti i pixel che hanno associato un numero di NA (secondo i
# criteri discussi per email) inferiore o uguale al numero specificato in max_missing_periods.

# Fine script esempio
################################################################################
