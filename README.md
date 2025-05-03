# BirdCommunities_Roads

## Description

This repository contains the code and data used in our study on the *Effects of linear openings in forest canopy on temperate bird communities*.


## Dependencies

The analysis was performed using R version R version 4.4.2. 

All packages used are listed in `requirements.txt`. You can install it directly from the CRAN for example with the R function `install.packages("package_name")`, except for the package `ggvegan` that require the installation of the `devtools` package, and then the installation of `ggvegan` from the GitHub repository with `devtools::install_github("gavinsimpson/ggvegan")`. A complet script to install all the packages is provided below.

If you want the exact same versions of the packages as used in the analysis, you may use the `remotes` package to install the packages from the `requirements.txt` file. You can run the following script. 

However, if you don't want to take care about the versions of the packages, you can run the script after commenting the lines with `install.packages("remotes")`, `remotes::install_version` and `devtools::install_github("gavinsimpson/ggvegan", ref = pkg_version)`, and uncomment the line with `install.packages(pkg_name)` and `devtools::install_github("gavinsimpson/ggvegan")`. 

```R
install.packages("remotes") # To get packages from CRAN archive
for (pkg in readLines("requirements.txt")) {
  
  pkg_name = strsplit(pkg, "==")[[1]][1] # Package name
  pkg_version = strsplit(pkg, "==")[[1]][2] # Package version
  
  if ( !(pkg_name %in% rownames(installed.packages())) ) { 
    # If the package is not installed
    if ( pkg_name %in% rownames(available.packages()) ) { 
        # If package is in CRAN
        # install.packages(pkg_name)
        remotes::install_version(pkg_name, version = pkg_version)
    } else if ( pkg_name == "ggvegan" ) { 
        # If package is ggvegan
        install.packages("devtools") # Install devtools
        # devtools::install_github("gavinsimpson/ggvegan")
        devtools::install_github("gavinsimpson/ggvegan", ref = pkg_version)
    } else { 
        # Else, we don't know where to find the package...
        message(paste("Package", pkg_name, "not found in CRAN or default Github repositories hardcoded."))
    }
  }
}
```