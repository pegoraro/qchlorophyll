################################################################################
#                    SCRIPT ESEMPIO K-MEANS ANALYSIS
################################################################################
# Note:
# Funziona con qchlorophyll versione 0.3
# Pacchetti aggiuntivi richiesti: ClusterSim, ggplot2, zoo

# Inizio script di esempio
################################################################################
# Set up

# Installo e carico la libreria qchlorophyll versione 0.3. (sostituire path e nome del .tar.gz,
# se venisse fornita la versione "source", aggiungere parametro type = "source")
install.packages("/home/qchlorophyll_0.3.tar.gz", repos = NULL)
require(qchlorophyll)

################################################################################
# Caricamento files di test

# Path
nc_files_path <- "/home/data_nc"
# Carico file .nc ed estraggo CHL1_mean
nc_files_list <- load_all_as_list(path = nc_files_path, variables = c("CHL1_mean"))
# Unisco il tutto in un unico dataframe.
nc_dataframe <- assign_id_and_melt(nc_files_list)

################################################################################
# Calcolo media

media <- aggregate_statistics(nc_dataframe,
                                   stat_funs = list(avg = "mean(., na.rm=TRUE)"),
                                   variable = "CHL1_mean")

################################################################################
# Reshape delle statistiche

media_reshaped <- reshape_statistics(media)

################################################################################
# Filtraggio dati mancanti

media_reshaped_less_NA <- filter_out_na(reshaped_data_list = media_reshaped,
                                             max_missing_periods =  2)

################################################################################
# Approssimzione dati mancanti (NA)

# Seleziono il dataframe che contiene i valori medi per ogni giorno per pixel
x <- media_reshaped_less_NA[[1]]

# Approssimo gli NA.
# Nota: di default (id_pixel, lon e lat sono esclusi dal processo, l'argomento
# "exclude variables" è ridondante in questo caso)
x <- approximate_NAs(data = x, exclude_variables = list("lon", "lat", "id_pixel"))

################################################################################
# Standardizzazione dati

# Nota: di default (id_pixel, lon e lat sono esclusi dal processo, l'argomento
# "exclude variables" è ridondante in questo caso)
x_stdz <- standardize_data(x, exclude_variables = list("lon", "lat", "id_pixel"))

################################################################################
# Analisi kmeans

# Centri scelti, n_centers: da 2 a 5
# Numero di iterazioni, nstart: 10
# Nota: ogni lista risultante dal kmeans contiene sia i centri standardizzati che i centri
# ripristinati alla loro scala orginale.
kmeans_results <- kmeans_analysis(x = x_stdz, n_centers = 2:5, nstart = 10, seed = 100)

################################################################################
# Plot dei risultati (Plot dell'indice di Calinski-Harabasz vs il numero di centri)

plot_results(kmeans_results)

################################################################################
# Scelta risultato ottimale in base all'indice di Calinski-Harabasz. (Risultato
# migliore == risultato che massimizza tale indice)

final_kmeans_results <- extract_results(kmeans_results)

# Fine script esempio
################################################################################
