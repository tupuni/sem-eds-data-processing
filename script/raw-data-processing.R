library(here)
library(tidyverse)

getwd()
# 1- combine raw data into single csv ----
#dir <- "2025-05-06_MEB-EMAE/data/raw_data"
dir <- "2025-06-12_MEB-EMAE/data/raw_data"

setwd(file.path("/Users/aymeric.hermann/GitHub/sem-eds-data-processing",dir))

### Pre-processing
list.files(pattern = "\\.csv$")

# Check all CSV files for row count consistency
system("bash -c 'for file in *.csv; do
  rows=$(tail -n +2 \"$file\" | wc -l)
  echo \"$file has $rows data rows\"
done'")

### Correct manually

### Remove the first 12 rows and transform into txt files from each CSV file 
system("for file in *.csv; do tail -n +13 \"$file\" > \"${file%.csv}.txt\"; done")

### Only keep the first 2 columns in the .txt files
# In a set of txt files with comma separated values, use bash command in R script to keep only the first two columns in each file, overwriting txt files
system("for file in *.txt; do cut -d',' -f1,2 \"$file\" > temp && mv temp \"$file\"; done")

### Transpose content of txt files, keep the same file names when saving files
system("
for file in *.txt; do
  awk -F',' '
  {
    for (i = 1; i <= NF; i++) {
      a[NR, i] = $i
    }
    if (NF > max_fields) max_fields = NF
  }
  END {
    for (i = 1; i <= max_fields; i++) {
      for (j = 1; j <= NR; j++) {
        printf \"%s\", a[j, i]
        if (j < NR) {
          printf \",\"
        }
      }
      printf \"\\n\"
    }
  }' \"$file\" > temp && mv temp \"$file\"
done
")

### Insert file name as 2nd row title
files <- list.files(pattern = "\\.txt$")

for (file in files) {
  fname <- sub("\\.txt$", "", file)  # remove .txt extension
  bash <- sprintf("
    awk -F',' -v fname='%s' '
    {
      if (NR == 2) {
        $1 = fname
      }
      for (i = 1; i <= NF; i++) {
        printf \"%%s\", $i
        if (i < NF) printf \",\"
      }
      printf \"\\n\"
    }' %s > temp && mv temp %s
  ", fname, shQuote(file), shQuote(file))
  
  system(bash)
}

### Combine all txt files
# Get all .txt files in the directory
files <- list.files(pattern = "\\.txt$")

# Output file name
combined_file <- "combined_output.txt"

# Track if we've written the header
first_file <- TRUE

# Clear the output file if it exists
if (file.exists(combined_file)) file.remove(combined_file)

for (file in files) {
  # Read all lines
  lines <- readLines(file)
  
  # Skip empty or 1-line files
  if (length(lines) < 2) next
  
  if (first_file) {
    # Write header and data
    cat(lines, sep = "\n", file = combined_file)
    first_file <- FALSE
  } else {
    # Write data only (skip header)
    cat(lines[-1], sep = "\n", file = combined_file, append = TRUE)
  }
}

### Read TXT and save to CSV
# read txt
output <- read.csv("combined_output.txt")
# remove duplicates
d <- output[!duplicated(output), ]
# fix col names
names(d) <- sub("\\.*K$", "", names(d))
names(d)[names(d) == "Element"] <- "id"
# recode ids by extracting the prefix before the final underscore
d$id <- sub("_\\d+$", "", d$id)


# 2-convert raw data ----

# Typically raw SEM data are provided in percentage of atomic weight (Wt %).
# These values can be converted into oxide weight percentages and ppm values using
# individual unified atomic mass unit (u) values
u <- data.frame(
  row.names = c('Si', 'Ti', 'Al', 'Fe', 'Ca', 'Mg', 'Mn', 'K', 'Na', 'P', 'O'),
  mass = c(28.085, 47.867, 26.982, 55.845, 40.078, 24.305, 54.938, 39.098, 22.990, 30.974, 15.999)
)
# and molar mass (m) values expressed in g/mol
m <- data.frame(
  row.names = c('SiO2', 'TiO2', 'Al2O3', 'FeO', 'Fe2O3', 'CaO', 'MgO', 'MnO', 'K2O', 'Na2O', 'P2O5'),
  mass = c(60.0830, 79.8650, 101.9610, 71.8464, 159.6922, 56.0774, 40.3044, 70.9374, 94.1956, 61.9785, 283.8860)
)

# conversion factors can be obtained by dividing the molar mass by the atomic mass
fact <- data.frame(
  row.names = c('Si-SiO2','Ti-TiO2','Al-Al2O3','Fe-FeO','Fe-Fe2O3',
                'Ca-CaO','Mg-MgO','Mn-MnO','K-K2O','Na-Na2O','P-P2O5'),
  factor = c(m['SiO2',]/u['Si',],
             m['TiO2',]/u['Ti',],
             m['Al2O3',]/(u['Al',]*2),
             m['FeO',]/u['Fe',],
             m['Fe2O3',]/(u['Fe',]*2),
             m['CaO',]/u['Ca',],
             m['MgO',]/u['Mg',],
             m['MnO',]/u['Mn',],
             m['K2O',]/(u['K',]*2),
             m['Na2O',]/(u['Na',]*2),
             m['P2O5',]/(u['P',]*2))
)
fact

# convert from wt% to oxides% by multiplying by the corresponding factor
# convert from wt% to ppm following 1% = 10000ppm (10^4)

names(d)
str(d)

d_conv <- d %>% 
  dplyr::mutate(
    'Na2O' = d[,'Na'] * fact['Na-Na2O',], 
    'MgO' = d[,'Mg'] * fact['Mg-MgO',], 
    'Al2O3' = d[,'Al'] * fact['Al-Al2O3',], 
    'SiO2' = d[,'Si'] * fact['Si-SiO2',], 
    'K2O' = d[,'K'] * fact['K-K2O',], 
    'CaO' = d[,'Ca'] * fact['Ca-CaO',], 
    'TiO2' = d[,'Ti'] * fact['Ti-TiO2',], 
    'MnO' = d[,'Mn'] * fact['Mn-MnO',], 
    'FeO' = d[,'Fe'] * fact['Fe-FeO',], 
    'Fe2O3' = d[,'Fe'] * fact['Fe-Fe2O3',]
  ) %>%
  dplyr::select(
    "id","SiO2","TiO2","Al2O3","Fe2O3",
    "CaO","MgO","K2O","Na2O")

# summarise and average
d_sum <- d_conv %>% 
  group_by(id) %>% 
  summarise_at(vars('SiO2':'Na2O'), mean, na.rm = F)

# save to CSV
#dir <- "2025-05-06_MEB-EMAE/data"
dir <- "2025-06-12_MEB-EMAE/data"

setwd(file.path("/Users/aymeric.hermann/GitHub/sem-eds-data-processing",dir))
getwd()
write.csv(d_sum, "combined_output_conv.csv", row.names = FALSE)

