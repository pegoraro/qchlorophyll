################################################################################
#                    SCRIPT ESEMPIO K-MEANS ANALYSIS
################################################################################
# Note:
# Funziona con qchlorophyll versione 0.3
# Pacchetti aggiuntivi richiesti: ClusterSim, ggplot2

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
                                   stat_funs = list(avg = "mean(., na.rm=TRUE)",
                                                    sdv = "sd(., na.rm=TRUE)"),
                                   variable = "CHL1_mean")

################################################################################
# Reshape delle statistiche

media_reshaped <- reshape_statistics(media)

################################################################################
# Filtraggio dati mancanti

media_reshaped_less_NA <- filter_out_na(reshaped_data_list = media_reshaped,
                                             max_missing_periods =  2)

################################################################################
# Standardizzazione dati

x <- media_reshaped_less_NA[[1]]
x_stdz <- standardize_data(x, exclude_variables = list("lon", "lat", "id_pixel"))

################################################################################
# Plot indice

optimal_clusters_number(x = x, minC = 2, maxC = 5, plot_show = TRUE)

################################################################################
# Run k-means analysis
kmeans_results <- kmeans_analysis(x = x, n_centers = 2:5, random_seed = 100)

# Fine script esempio
################################################################################
