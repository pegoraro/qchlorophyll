## Qchlorophyll

R Package for geographical chlorophyll analysis.

The package provides the following functions:

- A set of functions for loading and extracting geographical data from .nc files.
- A set of functions for cleaning the data and calculating any user-defined descriptive statistics/index.
- A set of functions for running k-means on the data and extracting information from the analysis.
- A set of functions for imputing missing data. 
- A set of functions for loading sets of yearly .csv files grouped in local folders.
- A set of functions for fitting a random forest model, getting information from the fitted model, predicting and plotting predictions in a geographical map.

Requires the following packages:

- ncdf4
- dplyr (>= 0.4.3)
- tidyr (>= 0.4.1)
- lubridate (>= 1.5.6)
- ggplot2 (>= 2.1.0)
- clusterSim
- mice
- lattice
- randomForest (>= 4.6-12)
- grid (>= 3.2.2)
- lazyeval (>= 0.1.10)
 
## Examples
The scripts folder contains some examples of use.
See the documentation of each function for more information.
