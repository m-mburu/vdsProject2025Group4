Group 4 Football Visualization( FC Barcelona 2008–2016 Analysis Shiny
App)
================
2025-05-15

This Shiny dashboard is for visualization of European football with a
focus of helping FC Baracelona prepare for 2017 season

### Prerequisites

1.  **R** (version 4.0.0 or higher)
2.  The file `data/all_obj_shiny.rda` must be in a folder named `data`
    at the project root.
3.  **Internet access** (to install missing packages).

### Required R Packages

We’ll use the following packages:

``` r
pkg_df <- read.csv("pkg_df.csv")
knitr::kable(pkg_df, caption = "Required R Packages for the Shiny App")
```

| Package        | Version |
|:---------------|:--------|
| viridis        | 0.6.5   |
| viridisLite    | 0.4.2   |
| ggbeeswarm     | 0.7.2   |
| sf             | 1.0-21  |
| DT             | 0.33    |
| data.table     | 1.17.0  |
| lubridate      | 1.9.4   |
| forcats        | 1.0.0   |
| stringr        | 1.5.1   |
| dplyr          | 1.1.4   |
| purrr          | 1.0.4   |
| readr          | 2.1.5   |
| tidyr          | 1.3.1   |
| tibble         | 3.2.1   |
| tidyverse      | 2.0.0   |
| plotly         | 4.10.4  |
| ggplot2        | 3.5.1   |
| leaflet        | 2.2.2   |
| shinydashboard | 0.7.3   |
| shiny          | 1.10.0  |

Required R Packages for the Shiny App

``` r
packages <- pkg_df$Package
packages
```

    ##  [1] "viridis"        "viridisLite"    "ggbeeswarm"     "sf"            
    ##  [5] "DT"             "data.table"     "lubridate"      "forcats"       
    ##  [9] "stringr"        "dplyr"          "purrr"          "readr"         
    ## [13] "tidyr"          "tibble"         "tidyverse"      "plotly"        
    ## [17] "ggplot2"        "leaflet"        "shinydashboard" "shiny"

### Setup Instructions

1.  **Clone or download** this repository to your machine.

2.  Click .Rproj file to open the project in RStudio

3.  **Install** the required packages by copying the helper function
    below into your R session (or at the top of your `app.R`), then run
    it:

    ``` r
    installPackages <- function(pkgs) {
      for (pkg in pkgs) {
        if (!pkg %in% rownames(installed.packages())) {
          message(sprintf("Installing missing package: %s", pkg))
          install.packages(pkg, dependencies = TRUE)
        } else {
          message(sprintf("Package already installed: %s", pkg))
        }
        library(pkg, character.only = TRUE)
      }
    }

    # Run to install & load:
    installPackages(packages)
    ```

4.  In R console the following

    ``` r
    library(shiny)
    runApp()
    ```

### Running via RStudio

1.  In RStudio, open `ui.R or server.R or app.r` file in the editor
    pane.
2.  Click the **Run App** button (top-right of the editor pane). A new
    window or RStudio viewer pane will open with the dashboard.
