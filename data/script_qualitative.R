# path <- "/Users/zauner/Library/CloudStorage/OneDrive-TUM/2024_Paper_Survey_Usability/Analysis/data/cleaned/LightLoggerFormFacto_CLEANED_DATA_2024-02-19.csv"
# 
# data <- read.csv(path)
library(tidyverse)
library(gt)
# data_qual <- 
# data |> select(contains("difficulty"), contains("check"), contains("openended"), contains("feedback"))
# 
# data_qual |> write_csv("/Users/zauner/Library/CloudStorage/OneDrive-TUM/2024_Paper_Survey_Usability/Analysis/data/cleaned/LightLogger_qual.csv")


#getting the input and output data of the analysis
data_qual <- read_csv("/Users/zauner/Library/CloudStorage/OneDrive-TUM/2024_Paper_Survey_Usability/Analysis/data/cleaned/LightLogger_qual.csv")
data_anal <- read_csv("/Users/zauner/Library/CloudStorage/OneDrive-TUM/2024_Paper_Survey_Usability/Analysis/data/raw/qual_analysis.txt")

#bring the output into the long form
data_analysis_long <- 
data_anal |> separate_longer_delim(`Supporting Quotes`, ";")

#check for non-direct comparisons
data_analysis_long <-
data_analysis_long |> 
  tibble::add_column(instances = {
data_analysis_long$`Supporting Quotes` |> 
  set_names() |> 
  map_vec(
    \(x) data_qual |> 
      filter(if_any(everything(), \(y) y == x)) |> nrow()
  )
  }) 

#checking for matches in the original
data_analysis_long|> 
  filter(instances == 0)

#>> looking at samples of the comparisons without direct matches, all could be found and were simply partial matches, so other parts of the answer were not quoted, as they did not relate to this aspect

data_analysis_long |> filter(Design == "Wrist") |> print(n = Inf)

#count the number of answers
data_analysis_long |> 
  mutate(Design = fct_inorder(Design),
    instances = ifelse(instances == 0, 1, instances)) |> 
  count(Design, Aspect, Explanation) |> 
  group_by(Design) |> 
  gt() |> 
  tab_header("Thematic analysis") |> 
  cols_width(Explanation ~ 500) |> 
  gtsave("/Users/zauner/Library/CloudStorage/OneDrive-TUM/2024_Paper_Survey_Usability/Analysis/output/02_tables/table_5_thematic.png", vwidth = 2000)
