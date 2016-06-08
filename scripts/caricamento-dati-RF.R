# Funziona con qchlorophyll ver 0.4
require(qchlorophyll)

setwd("/home/mich/quantide/packages_R/qchlorophyll_/dati/dati_random_forest")
path <- getwd()

# df di riferimento (è quello dei cluster)
# new_x

# Step 1 caricamento lista
### PAR ok
dfs_par <- load_all_csv(main_folder_path = path, folder = "PAR", starts_with = "PAR_", years = c("2011", "2012", "2013", "2014"))

# Step 2 verifica coerenza lon - lat / aggiustamento
dfs_par <- format_lon_lat_list(df_list = dfs_par, reference_df = new_x, reformat = FALSE)

# Step 3 aggiunta id_pixel e gruppi cluster. Forse qui conviene fare join anche con batimetria.
# Faccio un join con l'id_pixel e il gruppo ottenuto dal kmeans. Poi dovro fare un join con le altre variabili.
# dopo faccio un rbind di tutti i df.
#dfs_par2 <- lapply(dfs_par, function(x) left_join(x, new_x, by = c("lon","lat")))
dfs_par2 <- add_id_pixel_and_groups(dfs_par, reference_dataframe = new_x)

# Step 4 rbind
dfs_PAR <- rbind_all(dfs_par2)

# Ultimo step poi, join tra tutti i dataframe ottenuti.

################################################################################
# Funziona per SIC quindi funziona anche per gli altri
### SIC #differenze
# pattern non noto
dfs_sic <- load_all_csv(main_folder_path = path, folder = "SIC", starts_with = "SIC_", years = c("2011", "2012", "2013", "2014"))
dfs_sic <- format_lon_lat_list(df_list = dfs_sic, variable = "sic", reference_df = new_x)
dfs_sic2 <- add_id_pixel_and_groups(dfs_sic, new_x)

################################################################################
### SSS #differenze
# Valori di lon e lat shiftati di 0.125. (dfs_sss[[1]]$lat - new_x$lat)[1] ritorna 0.125
dfs_sss <- load_all_csv(main_folder_path =  path, folder = "SSS", starts_with = "SSS_", years = c("2011", "2012", "2013", "2014"))

### SST differenze
# pattern non noto
dfs_sst <- load_all_csv(main_folder_path = path, folder = "SST", starts_with = "SST_", years = c("2011", "2012", "2013", "2014"))

### WS differenze
# pattern non noto
dfs_ws <- load_all_csv(main_folder_path = path, folder = "WS", starts_with = "WIND_", years = c("2011", "2012", "2013", "2014"))

### BS ok
dfs_bs <- load_all_csv(main_folder_path = path, folder = "BS", starts_with = "BS_", years = c("2011", "2012", "2013", "2014"))

### BAT ok
bat <- read.csv("BAT/BAT.csv")


a <- format_lon_lat_list(df_list = dfs_ws, new_x,variable = "wind")
