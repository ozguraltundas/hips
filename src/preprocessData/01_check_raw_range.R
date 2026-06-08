library(readr)
library(readxl)
library(tidyverse)

#----------------------------------------------------------
# 1. Read current HIPS inbred file directly from GitHub
#----------------------------------------------------------

plot_level <- read_csv("https://raw.githubusercontent.com/jdavis-132/hips/master/finalData/HIPS_INBREDS_V12_1.csv")

lincoln_index <- read_xlsx("/home/schnablelab/PROJECTS/RAWDATA/data/May29_Data/220725 Inbred HIPS - SAM - Lincoln 2022 - Turkus Summary.xlsx",
  sheet = "Index")

#----------------------------------------------------------
# 3. Prepare Lincoln lookup table
#----------------------------------------------------------

lincoln_row_range <- lincoln_index %>%
  mutate(
    plotNumber = as.numeric(`Plot ID`),
    genotype_lower_case = str_squish(tolower(as.character(`Genotype (with @ removed)`))),
    row_new = as.numeric(Row),
    range_new = as.numeric(Range)
  ) %>%
  select(plotNumber, genotype_lower_case, row_new, range_new) %>%
  distinct()

#----------------------------------------------------------
# 4. Prepare plot-level data for matching
#----------------------------------------------------------

plot_level_for_join <- plot_level %>% mutate(
    plotNumber = as.numeric(plotNumber),
    genotype_lower_case = str_squish(tolower(as.character(genotype)))
  )

#----------------------------------------------------------
# 5. Data investigation: check missing row/range records
#----------------------------------------------------------

pl_check <- plot_level_for_join %>%
  filter(is.na(row) | is.na(range)) %>%
  select(year, location, row, range, plotNumber, genotype, genotype_lower_case) %>%
  left_join(lincoln_row_range, by = c("plotNumber", "genotype_lower_case")) %>%
  mutate(found_in_lincoln_index = !is.na(row_new) & !is.na(range_new))

# Check result
table(pl_check$found_in_lincoln_index) ## TRUE 127 

#----------------------------------------------------------
# 6. Fill missing row/range in original plot-level data
#----------------------------------------------------------

plot_level_fixed <- plot_level_for_join %>%
  left_join(lincoln_row_range, by = c("plotNumber", "genotype_lower_case")) %>%
  mutate(
    row = ifelse(is.na(row) & !is.na(row_new), row_new, row),
    range = ifelse(is.na(range) & !is.na(range_new), range_new, range)
  )
  

#----------------------------------------------------------
# 7. Check row number did not change
#----------------------------------------------------------

dim(plot_level) # 12829    52
dim(plot_level_fixed) # 12829    55
setdiff(colnames(plot_level_fixed), colnames(plot_level)) # "genotype_lower_case" "row_new"             "range_new"

plot_level_fixed <- plot_level_fixed %>% select(-genotype_lower_case, -row_new, -range_new)
setdiff(colnames(plot_level_fixed), colnames(plot_level)) # character(0)
#----------------------------------------------------------
# 8. Check remaining missing row/range
#----------------------------------------------------------

plot_level_fixed %>%
  filter(is.na(row) | is.na(range)) %>%
  select(year, location, row, range, plotNumber, genotype) ## none


#----------------------------------------------------------
# 9. Save new version
#----------------------------------------------------------

write_csv(
  plot_level_fixed,
  "/home/schnablelab/PROJECTS/RAWDATA/data/May29_Data/HIPS_INBREDS_V12_2.csv"
)
