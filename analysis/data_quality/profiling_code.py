# For instructions on running the script please see the 'instructions.txt' file. 
import pandas as pd
from pandas_profiling import ProfileReport
import glob2
import re 

# Currently all datasets in 'curated' are csvs but could amend this code to pick up excel/json too
file_paths = glob2.glob('../../../data/data-lake/curated/*/*.csv', recursive = True)

# Loop through the files and create pandas profiling reports for each of the csv datasets 
# and save them into the 'quality_reports/curated' folder
for file_path in file_paths:
  data = pd.read_csv(file_path)
  profile = ProfileReport(data, title="Pandas Profiling Report")
  file_name = re.search(r'([\w-]+.csv)$', file_path).group(1)
  file_name = file_name.replace(".csv", "")
  profile.to_file("quality_reports/curated/" + file_name + ".html")

