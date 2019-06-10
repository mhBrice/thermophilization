# thermophilization

This repository includes the data and R scripts to reproduce analyses and figures found in the article *Disturbances amplify tree community responses to climate change in the temperate-boreal ecotone* by Brice et al. in GEB.


## Installation

The analysis was carried out with [R (a free software environment for statistical computing and graphics)](https://www.r-project.org/) and required the installation of a recent version of it.

Analysis was reproduced in two different environments:

1. MacOSX Mojave

<details>
 add session info
</details>


2. Debian Buster

<details>
R version 3.5.2 (2018-12-20)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 10 (buster)

Matrix products: default
BLAS: /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] raster_2.8-19       sp_1.3-1            kableExtra_1.1.0    knitr_1.23         
 [5] effects_4.1-0       FD_1.0-12           geometry_0.4.1      ape_5.3            
 [9] ade4_1.7-13         gtools_3.8.1        car_3.0-2           carData_3.0-2      
[13] graphicsutils_1.3-0 scales_1.0.0        RColorBrewer_1.1-2  sf_0.7-3           
[17] zoo_1.8-4           vegan_2.5-4         lattice_0.20-38     permute_0.9-5      
[21] adespatial_0.3-4    reshape2_1.4.3      dplyr_0.8.0.1      

loaded via a namespace (and not attached):
  [1] minqa_1.2.4         colorspace_1.4-0    seqinr_3.4-5        deldir_0.1-16      
  [5] class_7.3-15        rio_0.5.16          estimability_1.3    rstudioapi_0.9.0   
  [9] xml2_1.2.0          codetools_0.2-16    splines_3.5.2       nloptr_1.2.1       
 [13] phylobase_0.8.6     cluster_2.0.7-1     shiny_1.2.0         readr_1.3.1        
 [17] compiler_3.5.2      httr_1.4.0          adegraphics_1.0-15  assertthat_0.2.0   
 [21] Matrix_1.2-17       lazyeval_0.2.2      survey_3.35-1       later_0.8.0        
 [25] htmltools_0.3.6     prettyunits_1.0.2   tools_3.5.2         igraph_1.2.4.1     
 [29] coda_0.19-2         gtable_0.2.0        glue_1.3.0          gmodels_2.18.1     
 [33] Rcpp_1.0.0          cellranger_1.1.0    spdep_1.1-2         gdata_2.18.0       
 [37] nlme_3.1-137        xfun_0.7            stringr_1.4.0       adephylo_1.1-11    
 [41] rvest_0.3.2         openxlsx_4.1.0      lme4_1.1-21         mime_0.6           
 [45] XML_3.98-1.19       LearnBayes_2.15.1   MASS_7.3-51.1       hms_0.4.2          
 [49] promises_1.0.1      parallel_3.5.2      expm_0.999-3        curl_3.3           
 [53] ggplot2_3.1.0       latticeExtra_0.6-28 stringi_1.4.3       highr_0.7          
 [57] plotrix_3.7-4       e1071_1.7-0.1       boot_1.3-20         zip_2.0.2          
 [61] spData_0.3.0        rlang_0.3.1         pkgconfig_2.0.2     rncl_0.8.3         
 [65] evaluate_0.13       purrr_0.3.2         tidyselect_0.2.5    plyr_1.8.4         
 [69] magrittr_1.5        R6_2.4.0            DBI_1.0.0           pillar_1.3.1       
 [73] haven_2.1.0         foreign_0.8-71      mgcv_1.8-27         units_0.6-2        
 [77] survival_2.43-3     abind_1.4-5         nnet_7.3-12         tibble_2.1.1       
 [81] crayon_1.3.4        uuid_0.1-2          KernSmooth_2.23-15  rmarkdown_1.12     
 [85] progress_1.2.0      RNeXML_2.3.0        adegenet_2.1.1      grid_3.5.2         
 [89] readxl_1.3.1        data.table_1.12.0   forcats_0.4.0       webshot_0.5.1      
 [93] digest_0.6.18       classInt_0.3-1      xtable_1.8-3        tidyr_0.8.3        
 [97] httpuv_1.5.1        munsell_0.5.0       viridisLite_0.3.0   magic_1.5-9

</details>


The following packages must be installed:

- dplyr
- plyr
- reshape2
- stringr
- sf
- sp
- raster
- vegan
- FD
- adespatial
- car
- gtools
- zoo
- effects
- RColorBrewer
- scales
- plotrix
- [graphicsutils](https://github.com/inSileco/graphicsutils) (not on CRAN)
- knitr
- kableExtra


Below are the R commands to install them all:

```R
install.packages(
  "dplyr", "plyr", "reshape2", "stringr", "sf", "sp", "raster", "vegan", "FD",
  "adespatial", "car", "gtools", "zoo", "effects", "RColorBrewer", "scales",
  "plotrix", "knitr", "kableExtra", "remotes"
)
remotes::install_github("inSileco/graphicsutils")
```


## Guidelines

All data used for the analyses can be found in the data folder.
Several steps required for data preparation were performed using scripts available in the [Quebec_data repository](https://github.com/mhBrice/Quebec_data). Scripts 1 and 2 format the data for the analyses performed in the scripts 3, 4 and 5. The figures produced are in the folder ms/figures.
