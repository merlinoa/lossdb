lossdb
========================================================

`lossdb` is an R package for storing and manipulating insurance loss data.  Many variables in insurance loss data sets can be loosely grouped into categories.  `lossdb` takes advantage of these common categories and uses them to ease manipulation of insurance loss data sets.  Additional functions are included for commonly desired manipulation, visualization, and data validation tasks.  

Package installation:
  
  ```R
  devtools::install_github("merlinoa/lossdb", build_vignettes = TRUE)
  ```
  
After installation, explore the introduction vignette to get a better feel for the goal and current capabilities of the package.

 ```R
 vignette("introduction", package = "lossdb")
 ```
