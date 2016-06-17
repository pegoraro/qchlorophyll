### SIC: lon e lat differenze pattern non noto
### SSS: differenze! lon e lat shiftati di 0.125. (dfs_sss[[1]]$lat - new_x$lat)[1] ritorna 0.125
### SST: differenze! lon e lat differenze pattern non noto
### WS: differenze! lon e lat differenze pattern non noto
### BS: lon e lat ok
### BAT: lon e lat ok

########################################################################################
# Funziona con qchlorophyll ver 0.4

#install.packages("/home/qchlorophyll_0.4.tar.gz", repos = NULL, type = "source")

require(qchlorophyll)

setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/dati_random_forest")
path <- getwd()

################################################################################
# Carico df di riferimento (è quello dei cluster esportabile direttamente in .csv
# con la nuova funzione qcholorphyll::export_data)
new_x <- load_a_single_csv("gruppi_kmeans.csv")

################################################################################
# Carico tutti i csv annuali

# Ottengo una lista di dataframe per ogni chiamata.
dfs_bs <- load_all_csv(main_folder_path = path, folder = "BS", starts_with = "BS_", years = c("2011", "2012", "2013", "2014"))
dfs_ws <- load_all_csv(main_folder_path = path, folder = "WS", starts_with = "WIND_", years = c("2011", "2012", "2013", "2014"))
dfs_sst <- load_all_csv(main_folder_path = path, folder = "SST", starts_with = "SST_", years = c("2011", "2012", "2013", "2014"))
dfs_sss <- load_all_csv(main_folder_path =  path, folder = "SSS", starts_with = "SSS_", years = c("2011", "2012", "2013", "2014"))
dfs_sic <- load_all_csv(main_folder_path = path, folder = "SIC", starts_with = "SIC_", years = c("2011", "2012", "2013", "2014"))
dfs_par <- load_all_csv(main_folder_path = path, folder = "PAR", starts_with = "PAR_", years = c("2011", "2012", "2013", "2014"))

# Caricamento .csv singoli
bat <- load_a_single_csv(file_path = "BAT/BAT.csv")
clusters <- load_a_single_csv(file_path = "gruppi_kmeans.csv")

################################################################################
# Aggiustamento latitudine e longitudine (può richiedere un pò di tempo)

dfs_bs2 <- format_lon_lat_list(df_list = dfs_bs, variable = "bs", reference_df = new_x, reformat = FALSE)
dfs_ws2 <- format_lon_lat_list(df_list = dfs_ws, variable = "wind", reference_df = new_x, reformat = TRUE)
dfs_sst2 <- format_lon_lat_list(df_list = dfs_sst, variable = "sst", reference_df = new_x, reformat = TRUE)
dfs_sss2 <- format_lon_lat_list(df_list = dfs_sss, variable = "sss", reference_df = new_x, shift = TRUE)
dfs_sic2 <- format_lon_lat_list(df_list = dfs_sic, variable = "sic", reference_df = new_x, reformat = TRUE)
dfs_par2 <- format_lon_lat_list(df_list = dfs_par, variable = "par", reference_df = new_x, reformat = FALSE)

################################################################################
# Aggiunta id_pixel e gruppo clustering

dfs_bs3 <- add_id_pixel_and_groups(dfs_bs2, reference_dataframe = new_x)
dfs_ws3 <- add_id_pixel_and_groups(dfs_ws2, reference_dataframe = new_x)
dfs_sst3 <- add_id_pixel_and_groups(dfs_sst2, reference_dataframe = new_x)
dfs_sss3 <- add_id_pixel_and_groups(dfs_sss2, reference_dataframe = new_x)
dfs_sic3 <- add_id_pixel_and_groups(dfs_sic2, reference_dataframe = new_x)
dfs_par3 <- add_id_pixel_and_groups(dfs_par2, reference_dataframe = new_x)

################################################################################
# Unione in un dataframe unico

final_df <- join_data(multiple_year_data = list(dfs_bs2, dfs_ws2, dfs_sst2, dfs_sss2, dfs_sic2, dfs_par2),
                      single_year_data = list(bat, new_x))
#View(final_df)

# Teniamo solo i pixel dove abbiamo fatto il clustering
final_df <- keep_pixels_with_group(final_df, group_name = "gruppo")

# Rimuoviamo variabili non più usate
rm(dfs_par3,dfs_sic3,dfs_sss3,dfs_sst3,dfs_ws3,dfs_bs3,
   dfs_par2,dfs_sic2,dfs_sss2,dfs_sst2,dfs_ws2,dfs_bs2,
   dfs_par,dfs_sic,dfs_sss,dfs_sst,dfs_ws,dfs_bs,
   bat,clusters)

################################################################################
################################################################################
# Modello random forest

# Fit del modello: 1000 alberi sono più che sufficienti (si vede dal plot sotto).
model <- fit_random_forest(formula = bs~.,
                           data = final_df,
                           seed = 500,
                           ntree = 1000,
                           do.trace = TRUE,
                           na.action = na.omit,
                           mtry = 11)

# Plot errore vs numero alberi
plot_error_vs_trees(rf_model = model, "Model error vs number of trees")

# Plot importanza variabili
get_variable_importance(rf_model = model)

# Plot importanza variabili
variable_importance_plot(rf_model = model)

# Ottengo dati dipendenza parziale (in caso voglia manipolarli dopo)
pdep_data <- partial_dependence_plot(model, data = final_df, verbose = TRUE)

# Plots della dipendenza parziale su tutte le variabili
partial_dependence_plot(model, data = final_df, show_plots = TRUE, cols = 2)

# Plot dipendenza parziale su variabile a scelta ("sst" in questo esempio)
single_partial_dependence_plot(model, final_df, "sst", ylabel = "bs")

# Mappa predittiva. Previsione media su tutti gli anni.
mp1 <- predictive_map(model, final_df)
print(mp1)

# Mappa predittiva. Previsione per ogni anno e faceting.
mp2 <- predictive_map(model, final_df, facet_by_year = TRUE)
print(mp2)

# Fit random forest hyperparameter mtry
data_fit <- na.omit(final_df)
fit_x <- dplyr::select(ffd,year,wind,sst,sss,sic,par,depth,gruppo)
randomForest::tuneRF(x = fit_x, y = data_fit$bs, ntreeTry = 1000)

# Fine script di esempio
################################################################################
