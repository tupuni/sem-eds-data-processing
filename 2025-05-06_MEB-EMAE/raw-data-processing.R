library(here)
getwd()
# 1- combine raw data into single csv ----
dir <- "2025-05-06_MEB-EMAE/data/"
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
# save to CSV
write.csv(d, "combined_output.csv", row.names = FALSE)


# 2- visualize raw data ----
my_theme <- theme_classic() + 
  theme(axis.line=element_blank()) +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, linewidth = 1),
        legend.position="right"#, aspect.ratio=1
  )
library(viridis)

### check for potential outliers
# Compute variability (mean, sd, range) for each duplicated ID
var <- d %>%
  group_by(id) %>%
  summarise(
    mean_O = mean(O), sd_O = sd(O), range_O = max(O) - min(O),
    mean_Si = mean(Si), sd_Si = sd(Si), range_Si = max(Si) - min(Si),
    .groups = "drop"
    )
var %>% print(n=23)

# vizualise
var_O <- ggplot(d, aes(x = id, y = O, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "O Variability", y = "O", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_O

var_Si <- ggplot(d, aes(x = id, y = Si, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "Si Variability", y = "Si", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_Si

var_K <- ggplot(d, aes(x = id, y = K, fill = id)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers to avoid duplication
  geom_jitter(aes(color = id), width = 0.2, size = 1.5, alpha = 0.7) +  # adds points
  scale_fill_viridis_d(option = "D") +
  scale_color_viridis_d(option = "D") +  # match point color to fill
  labs(title = "K Variability", y = "K", x = "id") + my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.position = "none")
var_K

ggplot(data = d, aes(x = id, y = O/Si, group = id, color = id)) + 
  geom_point(size = 2) + scale_color_viridis_d(option = "D") + 
  my_theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = "none")

ggplot(data = var, aes(x = sd_O, y = sd_Si, group = id, color = id)) + 
  geom_point(size = 2) +
  scale_color_viridis_d(option = "D") +  # Use color scale, not fill
  labs(title = "O-Si variability correlation", y = "sd_Si", x = "sd_O") +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  scale_x_continuous(limits=c(0, 3.5)) + 
  scale_y_continuous(limits=c(0, 3.5)) +
  my_theme + theme(aspect.ratio=1, legend.position = "none")

#save
setwd("~/GitHub/sem-eds-data-processing/2025-05-06_MEB-EMAE")
require(patchwork)

pdf(("fig/var.pdf"), width=8, height=12)
var_O /
  var_Si /
  var_K
dev.off()





library(tidyverse)
# Group by id and average columns 2 to 11
d_avg <- d %>%
  group_by(id) %>%
  summarise(across(1:10, mean), .groups = "drop")

# Save to CSV
write.csv(final_df, "combined_transposed_output.csv", row.names = FALSE)

df <- data.frame(
  ID = c("sampleA_1", "sampleA_2", "sampleB_1", "sampleC_1", "sampleC_2", "sampleC_3"),
  Value = c(10, 15, 20, 5, 10, 15)
)
