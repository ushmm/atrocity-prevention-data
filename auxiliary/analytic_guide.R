### preamble
getwd()
work_dir <- "D:/Google Drive/CPG/Projects/Research/Lessons-Learned/Kyra's Folder/Lessons learned/1. Collecting, Distilling, and Organizing the Knowledge/Empirical Knowledge/4. NEW coding process/Analytic guide"
setwd(work_dir)

packages <- c("reshape", "plyr", "dplyr", "car", "stargazer", "gridExtra", "olsrr", 
              "foreign", "ggplot2", "ggmap", "mapsapi", "sf", "sp", "data.table", 
              "mapdata", "maps", "raster", "rworldmap", "GADMTools", "rgdal", "nngeo", 
              "mapview", "plm", "gplots", "haven", "lfe", "plm", 
              "haven", "knitr", "AER", "DataCombine", "jtools", "maptools", "mapdata",
              "rgeos", "geosphere", "tidyr", "coefplot", "margins", "fuzzyjoin") # combines packages
lapply(packages, library, character.only = TRUE) # loads all packages in "packages" list
rm(packages, work_dir)

#######################################################

### scrape analytic guide and reformat

#######################################################

### load Word doc using OFFICER
library(officer)
doc_analytic_guide <- read_docx("[for editing] LL analytic guide to coding.docx")

### Word doc as tibble
tbl_analytic_guide <- docx_summary(doc_analytic_guide) %>%
  as_tibble() %>%
  distinct()

### fill in header text by num_id
analytic_guide <- tbl_analytic_guide

analytic_guide <- analytic_guide %>%
  fill(num_id, .direction = "up")

### remove blanks
analytic_guide <- analytic_guide %>%
  subset(text != "")

### remove level, paragraph, content_type, style_name
analytic_guide <- analytic_guide %>%
  dplyr::select(doc_index, text, num_id)

### create category types
analytic_guide <- analytic_guide %>%
  separate(text, c("type", "text"), ": ")

### create category for tool and factor names (for all non-blank cells)
analytic_guide <- analytic_guide %>%
  mutate(text = ifelse(type != "" & is.na(text), type, text), 
         type = ifelse(type == text, "Name", type))

### pivot longer dataset to wider, fill in blanks by num_id, deduplicate
analytic_guide_wider <- analytic_guide %>%
  pivot_wider(names_from = type,
              values_from = text) %>%
  group_by(num_id) %>%
  fill(-num_id) %>%
  fill(-num_id, .direction = "up") %>%
  dplyr::select(-doc_index) %>%
  distinct()

### set NA values as NA
library(naniar)
analytic_guide_wider <- analytic_guide_wider %>%
  replace_with_na_all(condition = ~.x == "NA")

##########################################################################################

### create binary variables for each category of information with greater than two values

###########################################################################################

### strategies (for tools)
analytic_guide_wider <- analytic_guide_wider %>%
  mutate(dissuade = ifelse(grepl("dissuading", `Strategies the tool can support`), 1, 
                           ifelse(is.na(`Strategies the tool can support`), NA, 0)),
         degrade = ifelse(grepl("degrading", `Strategies the tool can support`), 1, 
                            ifelse(is.na(`Strategies the tool can support`), NA, 0)), 
         protect = ifelse(grepl("protecting", `Strategies the tool can support`), 1, 
                          ifelse(is.na(`Strategies the tool can support`), NA, 0)),
         transition = ifelse(grepl("transition", `Strategies the tool can support`), 1,
                             ifelse(is.na(`Strategies the tool can support`), NA, 0)))

### DIMEL (for tools)
analytic_guide_wider <- analytic_guide_wider %>%
  mutate(military = ifelse(grepl("Military", DIMEL), 1, 
                           ifelse(is.na(DIMEL), NA, 0)),
         diplomatic = ifelse(grepl("Diplomatic", DIMEL), 1, 
                             ifelse(is.na(DIMEL), NA, 0)),
         informational = ifelse(grepl("Informational", DIMEL), 1, 
                                ifelse(is.na(DIMEL), NA, 0)),
         economic = ifelse(grepl("Economic", DIMEL), 1, 
                           ifelse(is.na(DIMEL), NA, 0)),
         legal = ifelse(grepl("Legal", DIMEL), 1, 
                        ifelse(is.na(DIMEL), NA, 0)))

### family (for factors)    
analytic_guide_wider <- analytic_guide_wider %>%
  mutate(domestic_context = ifelse(grepl("Domestic context", Family), 1,
                                   ifelse(is.na(Family), NA, 0)),
         conflict_dynamics = ifelse(grepl("Conflict dynamics", Family), 1,
                                    ifelse(is.na(Family), NA, 0)),
         target_characteristics = ifelse(grepl("Target characteristics", Family), 1,
                                         ifelse(is.na(Family), NA, 0)),
         implementer_choices = ifelse(grepl("Implementer choices", Family), 1,
                                      ifelse(is.na(Family), NA, 0)),
         implementer_characteristics = ifelse(grepl("Implementer characteristics", Family), 1,
                                              ifelse(is.na(Family), NA, 0)),
         international_dynamics = ifelse(grepl("International dynamics", Family), 1,
                                         ifelse(is.na(Family), NA, 0)),
         tool_specific = ifelse(grepl("factors", Family), 1,
                                ifelse(is.na(Family), NA, 0)))

### write CSV
write.csv(analytic_guide_wider, "analytic_guide.csv")
