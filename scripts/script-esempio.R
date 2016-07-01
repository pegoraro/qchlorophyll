################################################################################
#                       SCRIPT CARICAMENTO DATI QCHLOROPHYLL
################################################################################
#

################################################################################
# Set up

# Installo qchlorophyll in locale
install.packages("/home/qchlorophyll.tar.gz", repos = NULL)

# Carico la libreria qchlorophyll
require(qchlorophyll)

################################################################################
# Caricamento files

# Definisco il percorso dove si trovano i file .nc da caricare
#
# Nota 1: la cartella puo' contenere anche file con estensione diversa da .nc o
# anche sottocartelle: la funzione load_all_as_list ignorera' questi oggetti e
# considerera' solo i file con estensione .nc nella cartella indicata (data_nc
# nell' esempio sotto).
nc_files_path <- "/home/mich/quantide/packages_R/qchlorophyll_/dati/CHL_8D"

# Carico TUTTI i file presenti nella cartella indicata.
#
# Nota 1: la funzione load_all_as_list ritorna una lista di dataframe. Ciascun
# dataframe contiene i dati di uno dei file .nc caricati
#
# Nota 2: la funzione si aspetta di default che ogni file contenga nel nome la data.
# per default, la prima data che compare nel nome viene considerata come data delle rilevazioni.
# Puoi cambiare questa opzione cambiando il parametro date_match.
#
# Nota 3: la funzione si aspetta di default un formato di data "yyyy-mm-dd". Puoi cambiare questa
# opzione nell'argomento date_format in caso in cui venisse cambiato il formato nel nome dei files.
# In generale tenderei a non toccare troppo questo parametro.
#
# Per maggiori informazioni
?load_all_as_list

nc_files_list <- load_all_as_list(path = nc_files_path)

# Carico tutti i file a partire da una certa data (inclusa).
# Nota 1: il formato della data "from" deve essere lo stesso dei file "yyyy-mm-dd" oppure "yyyymmdd" (ossia quello fissato nel parametro
# "date_format").
from <- "2005-01-01"
nc_files_list <- load_all_as_list(path = nc_files_path, from = from)

# Carico tutti i file fino a una certa data (inclusa).
to <- "2005-01-01"
nc_files_list <- load_all_as_list(path = nc_files_path, to = to)

# Carico tutti i file entro un intervallo di date
nc_files_list <- load_all_as_list(path = nc_files_path, from = from, to = to)

# Assegno id a ciascun pixel utilizzando come identificativo le coordinate e
# unisco il tutto in un unico dataframe.
nc_dataframe2 <- assign_id_and_melt(nc_files_list)

View(nc_dataframe)

