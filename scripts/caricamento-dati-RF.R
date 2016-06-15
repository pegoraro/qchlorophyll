### SIC: lon e lat differenze pattern non noto
### SSS: differenze! lon e lat shiftati di 0.125. (dfs_sss[[1]]$lat - new_x$lat)[1] ritorna 0.125
### SST: differenze! lon e lat differenze pattern non noto
### WS: differenze! lon e lat differenze pattern non noto
### BS: lon e lat ok
### BAT: lon e lat ok

########################################################################################
########################################################################################
# Funziona con qchlorophyll ver 0.4

require(qchlorophyll)

setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/dati_random_forest")
path <- getwd()

################################################################################
# Carico df di riferimento (è quello dei cluster)
new_x

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
# Aggiustamento latitudine e longitudine

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
# Row bind and join

final_df <- join_data(multiple_year_data = list(dfs_bs2, dfs_ws2, dfs_sst2, dfs_sss2, dfs_sic2, dfs_par2),
                      single_year_data = list(bat, new_x))
#View(final_df)

# Keep only pixels for which the group has been calculated. 1992 righe -> 492 pixel per 4 anni.
# Su 1992, 619 bloom start sono NA.......
final_df <- keep_pixels_with_group(final_df, group_name = "gruppo")

# Clean environment
rm(dfs_par3,dfs_sic3,dfs_sss3,dfs_sst3,dfs_ws3,dfs_bs3,
   dfs_par2,dfs_sic2,dfs_sss2,dfs_sst2,dfs_ws2,dfs_bs2,
   dfs_par,dfs_sic,dfs_sss,dfs_sst,dfs_ws,dfs_bs,
   bat,clusters)

################################################################################
# RF model

# Fit random forest. 2000 alberi sembrano più che sufficienti per avere mse piu o meno costante
model <- fit_random_forest(formula = bs~., data = final_df, seed = 500, ntree = 2000, do.trace = TRUE, na.action = na.omit)

# Plot of error vs number of trees
plot_error_vs_trees(model, "Model error vs number of trees")

# Plot of variable importance
get_variable_importance(rf_model = model)

# Plot of variable importance
variable_importance_plot(rf_model = model)

#--------------------
# Problemi da qui...

# Plot of partial variable dependence
partial_dependence_plot(model,final_df)
xa <- "sic"
a <- randomForest::partialPlot(x = model, pred.data = as.data.frame(final_df), x.var = "sic", plot=FALSE)

# Mappa predittiva
predictive_map(model, data_)
