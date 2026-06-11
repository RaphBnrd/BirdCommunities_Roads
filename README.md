# BirdCommunities_Roads

## Description

This repository accompanies:

> Yapu-Alcazar, M., Benerradi, R., Wohlwend, M., Mikusiński, G., Storch, I., & Bhardwaj, M. (2025). *Effects of linear openings in forests on temperate bird communities*. Ecology and Evolution, 15(11), e72466. https://doi.org/10.1002/ece3.72466

You can cite the publication with:

```bibtex
@article{YapuAlcazar2025,
  title = {Effects of Linear Openings in Forests on Temperate Bird Communities},
  volume = {15},
  ISSN = {2045-7758},
  url = {http://dx.doi.org/10.1002/ece3.72466},
  DOI = {10.1002/ece3.72466},
  number = {11},
  journal = {Ecology and Evolution},
  publisher = {Wiley},
  author = {Yapu‐Alcazar,  Mariela and Benerradi,  Raphaël and Wohlwend,  Michael and Mikusiński,  Grzegorz and Storch,  Ilse and Bhardwaj,  Manisha},
  year = {2025},
  month = Nov 
}
```

## Dependencies

The analysis was performed using R version 4.4.2.

All packages used are listed in `requirements.txt`. You can install them directly from CRAN using the R function `install.packages("package_name")`, except for the package `ggvegan`, which requires the installation of the `devtools` package and the command `devtools::install_github("gavinsimpson/ggvegan")`. A complete script to install all the packages is provided below.

If you want the exact same versions of the packages as used in our analysis, you may use the `remotes` package to install the packages from the `requirements.txt` file. You can run the script below.

However, if you don't want to take care about the versions of the packages, you can run the script after commenting the lines with `install.packages("remotes")`, `remotes::install_version`, and `devtools::install_github("gavinsimpson/ggvegan", ref = pkg_version)`, and uncomment the lines with `install.packages(pkg_name)` and `devtools::install_github("gavinsimpson/ggvegan")`.

```r
install.packages("remotes") # To get packages from CRAN archive
for (pkg in readLines("requirements.txt")) {
  pkg_name <- strsplit(pkg, "==")[[1]][1] # Package name
  pkg_version <- strsplit(pkg, "==")[[1]][2] # Package version

  if (!(pkg_name %in% rownames(installed.packages()))) {
    # If the package is not installed
    if (pkg_name %in% rownames(available.packages())) {
      # If package is in CRAN
      # install.packages(pkg_name)
      remotes::install_version(pkg_name, version = pkg_version)
    } else if (pkg_name == "ggvegan") {
      # If package is ggvegan
      install.packages("devtools") # Install devtools
      # devtools::install_github("gavinsimpson/ggvegan")
      devtools::install_github("gavinsimpson/ggvegan", ref = pkg_version)
    } else {
      # Else, we don't know where to find the package...
      message(paste("Package", pkg_name, "not found in CRAN or default GitHub repositories hardcoded."))
    }
  }
}
```
