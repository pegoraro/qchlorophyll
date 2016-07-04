################################################################################
#                    SCRIPT ESEMPIO K-MEANS ANALYSIS
################################################################################
# Note:
# Funziona con qchlorophyll versione 0.3
# Pacchetti aggiuntivi richiesti: ClusterSim, ggplot2, mice

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
nc_files_path_chl <- "/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D"
nc_files_path_pic <- "/home/mich/quantide/packages_R/qchlorophyll_/dati/PIC_TEST"
# Carico file .nc ed estraggo CHL1_mean
nc_files_list_chl <- load_all_as_list(path = nc_files_path_chl, variables = c("CHL1_mean"))
nc_files_list_pic <- load_all_as_list(path = nc_files_path_pic, variables = c("PIC_mean"))
# Unisco il tutto in un unico dataframe.
nc_dataframe_chl <- assign_id_and_melt(nc_files_list_chl)
nc_dataframe_pic <- assign_id_and_melt(nc_files_list_pic)

################################################################################
# Calcolo media

media_chl <- aggregate_statistics(nc_dataframe_chl,
                                   stat_funs = list(avg = "mean(., na.rm=TRUE)"),
                                   variable = "CHL1_mean")
media_pic <- aggregate_statistics(nc_dataframe_pic,
                                  stat_funs = list(avg = "mean(., na.rm=TRUE)"),
                                  variable = "PIC_mean")

################################################################################
# Reshape delle statistiche

media_reshaped_chl <- reshape_statistics(media_chl)
media_reshaped_pic <- reshape_statistics(media_pic)

################################################################################
# Filtraggio dati mancanti

media_reshaped_less_NA_chl <- filter_out_na(reshaped_data_list = media_reshaped_chl,
                                             max_missing_periods =  2)
media_reshaped_less_NA_pic <- filter_out_na(reshaped_data_list = media_reshaped_pic,
                                        max_missing_periods =  10)
################################################################################
# Approssimzione dati mancanti (NA)

# Seleziono il dataframe che contiene i valori medi per ogni giorno per pixel
chl <- media_reshaped_less_NA_chl[[1]]
pic <- media_reshaped_less_NA_pic[[1]]
View(chl)
View(pic)
# Approssimo gli NA.

# Nota: di default (id_pixel, lon e lat sono esclusi dal processo)
# L'imputazione è effettuata con il pacchetto mice.
# I valori vengono approssimati utilizzando il predictive mean matching.
# E' possibile scegliere il metodo con cui effettuare l'imputazione attraverso
# l'argomento "meth".  Vedere la documentazione della funzione mice::mice per maggiori informazioni
x_chl <- approximate_NAs(data = chl, seed = 500)
x_pic <- approximate_NAs(data = pic, seed = 500, exclude_variables = list("id_pixel"))

class(chl)
class(pic)
dim(chl)
dim(pic)

# Plot della densità dei dati approssimati vs densità dati reali.
# I dati approssimati sono quelli in viola, i dati reali quelli in blu.
# Nota: i punti dove mancano la maggior parte dei dati sono il d_024 e il d_312 cioè il primo e l'ultimo
# giorno giuliano della serie. Le distribuzioni dei dati approssimati sono molto simili a quelle dei dati
# reali in questi due casi. Negli altri casi mancano pochi dati.
plot_density_imputed_na(x_chl)

################################################################################
# Standardizzazione dati

x_stdz <- standardize_data(x_chl)

################################################################################
# Analisi kmeans

# Per ogni valore di n_centers, viene effettuata un'analisi di tipo k-means, e calcolato
# l'indice di Calinski-Harabasz. I risultati (del kmeans e l'indice) sono raccolti in una
# lista e ritornati.

# Vengono ritornati sia i centri standardizzati che quelli non standardizzati (non scalati).

# I centri standardizzati si ottengono nel seguente modo (es. per numero di centri pari a 2):
# kmeans_results$`2`$centers
# I centri NON standardizzati si ottengono nel seguente modo (es. per numero di centri pari a 2):
# kmeans_results$`2`$centers_not_scaled

# Esempio kmeans da 2 a 5 centri.

# Centri scelti, n_centers: da 2 a 5
# Numero di iterazioni, nstart: 10
# Nota: ogni lista risultante dal kmeans contiene sia i centri standardizzati che i centri
# ripristinati alla loro scala orginale.
kmeans_results <- kmeans_analysis(x = x_stdz, n_centers = 2:5, nstart = 100, seed = 100)

################################################################################
# Plot dei risultati (Plot dell'indice di Calinski-Harabasz vs il numero di centri)

plot_results(kmeans_results)

################################################################################
# Scelta risultato ottimale in base all'indice di Calinski-Harabasz. (Risultato
# migliore == risultato che massimizza tale indice)

final_kmeans_results <- extract_results(kmeans_results)

################################################################################
# Export dati kmeans nella current working directory

export_data(x, nc_dataframe, final_kmeans_results, "gruppi_kmeans.csv")

# Fine script esempio
################################################################################
