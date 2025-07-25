### load packages

groundhog.library("officer", "2022-07-13") ### manipulate Word docs
groundhog.library("dplyr", "2022-07-13") ### data wrangling 
groundhog.library("tidyr", "2022-07-13") ### data wrangling
groundhog.library("naniar", "2022-07-13") ### NAs

#######################################################

### scrape analytic guide and reformat

#######################################################

### load Word doc using OFFICER
doc_analytic_guide <- read_docx("analytic_guide_2023.docx")

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

### write CSV
write.csv(analytic_guide_wider, "analytic_guide_2023.csv")
