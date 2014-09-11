lossdb
========================================================

lossdb is an R package for storing and manipulating insurance loss data.  Many of variables in insurance loss data sets can be loosely grouped into categories.  lossdb takes advantage of these common categories and uses them to ease manipulation of the data sets.  Additional functions are included for commonly desired manipulation, visualization, and data validation.  
 
lossdb is still in very early stages of development, and this is my first R package.  I appreciate any advice.

Package installation:
  
  ```R
  devtools::install_github("merlinoa/lossdb")
  ```
  
After installation, explore the rough draft of the introduction vignette to get a better feel of the goal and current capabilities of the package.

 ```R
 vignette("introduction", package = "lossdb")
 ```
