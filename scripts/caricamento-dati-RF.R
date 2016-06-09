# Funziona con qchlorophyll ver 0.4
require(qchlorophyll)

setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/dati_random_forest")
path <- getwd()

##########################################################################################
##########################################################################################

# df di riferimento (è quello dei cluster)
# new_x

# Step 1 caricamento lista
### PAR: lon e lat ok
dfs_par <- load_all_csv(main_folder_path = path, folder = "PAR", starts_with = "PAR_", years = c("2011", "2012", "2013", "2014"))

# Step 2 verifica coerenza lon - lat / aggiustamento
dfs_par <- format_lon_lat_list(df_list = dfs_par, reference_df = new_x, reformat = FALSE)

# Step 3 aggiunta id_pixel e gruppi cluster. Forse qui conviene fare join anche con batimetria.
# Faccio un join con l'id_pixel e il gruppo ottenuto dal kmeans. Poi dovro fare un join con le altre variabili.
# dopo faccio un rbind di tutti i df.
#dfs_par2 <- lapply(dfs_par, function(x) left_join(x, new_x, by = c("lon","lat")))
dfs_par2 <- add_id_pixel_and_groups(dfs_par, reference_dataframe = new_x)

# Step 4 rbind e join
# da implementare

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

# Carico df di riferimento (è quello dei cluster)
# new_x

# Carico tutti i csv annuali

# Bloom start, wind, sst, sss, sic, par. Ottengo una lista di dataframe per ogni chiamata.
dfs_bs <- load_all_csv(main_folder_path = path, folder = "BS", starts_with = "BS_", years = c("2011", "2012", "2013", "2014"))
dfs_ws <- load_all_csv(main_folder_path = path, folder = "WS", starts_with = "WIND_", years = c("2011", "2012", "2013", "2014"))
dfs_sst <- load_all_csv(main_folder_path = path, folder = "SST", starts_with = "SST_", years = c("2011", "2012", "2013", "2014"))
dfs_sss <- load_all_csv(main_folder_path =  path, folder = "SSS", starts_with = "SSS_", years = c("2011", "2012", "2013", "2014"))
dfs_sic <- load_all_csv(main_folder_path = path, folder = "SIC", starts_with = "SIC_", years = c("2011", "2012", "2013", "2014"))
dfs_par <- load_all_csv(main_folder_path = path, folder = "PAR", starts_with = "PAR_", years = c("2011", "2012", "2013", "2014"))

# Caricamento .csv singoli

# Batimetria, clustering (dataframe di riferimento)
bat <- load_a_single_csv(file_path = "BAT/BAT.csv")
clusters <- load_a_single_csv(file_path = "gruppi_kmeans.csv")


# Aggiustamento latitudine e longitudine

dfs_bs2 <- format_lon_lat_list(df_list = dfs_bs, variable = "bs", reference_df = new_x, reformat=FALSE)
dfs_ws2 <- format_lon_lat_list(df_list = dfs_ws, variable = "wind", reference_df = new_x, reformat=TRUE)
dfs_sst2 <- format_lon_lat_list(df_list = dfs_sst, variable = "sst", reference_df = new_x, reformat=TRUE)
dfs_sss2 <- format_lon_lat_list(df_list = dfs_sss, variable = "sss", reference_df = new_x, shift = TRUE)
dfs_sic2 <- format_lon_lat_list(df_list = dfs_sic, variable = "sic", reference_df = new_x, reformat=TRUE)
dfs_par2 <- format_lon_lat_list(df_list = dfs_par, variable = "par", reference_df = new_x, reformat = FALSE)

# Aggiunta id_pixel e gruppo clustering
dfs_bs3 <- add_id_pixel_and_groups(dfs_bs2, reference_dataframe = new_x)
dfs_ws3 <- add_id_pixel_and_groups(dfs_ws2, reference_dataframe = new_x)
dfs_sst3 <- add_id_pixel_and_groups(dfs_sst2, reference_dataframe = new_x)
dfs_sss3 <- add_id_pixel_and_groups(dfs_sss2, reference_dataframe = new_x)
dfs_sic3 <- add_id_pixel_and_groups(dfs_sic2, reference_dataframe = new_x)
dfs_par3 <- add_id_pixel_and_groups(dfs_par3, reference_dataframe = new_x)


# Row bind and join. Da implementare.
#final_df <- unify_data(multiyear_df = list(dfs_par3, ...), single_values_df = list(bat), reference_df = new_x )
