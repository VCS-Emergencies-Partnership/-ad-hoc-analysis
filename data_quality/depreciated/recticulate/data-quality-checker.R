# Had issues with using pandas profiling via reticulate (worked on local computer but not on DSVM)
# Please use the python script 'profiling_code.py' 
pathway_to_dataset <- "../"
data <- read_csv(pathway_to_dataset)

library(reticulate)
#have set up a cona environment names 'data_quaility_checker' for this project which has 'pandas_profiling' installed on it
#have set this up in the .Rprofile file to point to this conda environment (see https://github.com/rstudio/reticulate/issues/292)

#ensure that using the correct environment. 
reticulate::py_config()
#reticulate::py_discover_config()
conda_list() 


#testing another package (not pandas profiling)
np <-  reticulate::import("numpy", convert = FALSE)
np$arange(1, 9)$reshape(2L, 2L, 2L)
sns <- reticulate::import('seaborn', convert = FALSE)

#trying pandas profiling
pandas_profiling <- reticulate::import("pandas_profiling", convert = FALSE)
py_run_string('from pandas_profiling import ProfileReport')

                             
profile <-
  pandas_profiling$ProfileReport(
    data,
    title = "Pandas Profiling Report"
  )