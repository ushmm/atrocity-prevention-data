### load packages
library(dplyr) ### data wrangling
library(stringr) ### edit strings
library(tidyr) ### data wrangling
library(fuzzyjoin) ### fuzzy text joins
library(reshape) ### reshape datasets
library(ggplot2) ### plots

### set working directory
work_dir <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/V1"
setwd(work_dir)

################################

### load and subset data

################################

### load excerpt-level data
excerpts <- read.csv("excerpts_1.csv", na.strings = "", check.names = F) %>%
  dplyr::select(-330)

### change variable names with spaces to periods
names(excerpts) <- gsub(x = names(excerpts),
                        pattern = " ",
                        replacement = ".") 

### change "True" and "False" to "1" and "0", respectively
excerpts[excerpts == "True"] <- 1
excerpts[excerpts == "False"] <- 0

### subset to remove NA rows
excerpts <- excerpts %>%
  subset(!is.na(Excerpt.Creator))

### 2. code_guide with key to correct coders
code_guide <- read.csv("code_guide_1.csv", na.strings = "")

### 3. master list of codes, remove Category and Sub.category columns
codes <- read.csv("master_codes_1.csv")
codes[, c("Sub.category", "X")] <- NULL

### left join tool dataframe with list of correct coders
excerpts <- left_join(excerpts, code_guide[, c("article_index", "coder_1", "coder_2", "coder_3", "Excluded.")],
                      by = c("Index.Number" = "article_index"),
                      keep = TRUE)
  
### identify correct coders
excerpts <- excerpts %>%
  mutate(correct_coder = ifelse(article_index == Index.Number & Excerpt.Creator == coder_1, 1, 
                                ifelse(article_index == Index.Number & Excerpt.Creator == coder_2, 1,
                                       ifelse(article_index == Index.Number & Excerpt.Creator == coder_3, 1, 0))))

### subset to remove incorrect coders
excerpts <- excerpts %>% 
  subset(correct_coder == 1)

### subset to remove excluded articles
excerpts <- excerpts %>%
  subset(Excluded. == 0)

### remove periods and backslashes from column names
excerpts <- excerpts %>% 
  dplyr::rename_all(
    ~str_replace_all(., "\\.", " ")
  ) %>%
  dplyr::rename_all(
    ~str_replace_all(., fixed("\\"), " ")
  )

### create index for excerpt
excerpts$excerpt_index <- seq(1:nrow(excerpts))

### pivot wider to longer
excerpts_long <- excerpts %>%
  pivot_longer(cols = starts_with("Code:"),
               names_to = "code",
               names_prefix = "Code:",
               values_to = "applied")

### remove extraneous Excerpt Creator, Codes.Applied.Combined, Title, Author, Year, X variables
excerpts <- excerpts_long
rm(excerpts_long)

excerpts <- excerpts[, c("Index Number",
                         "excerpt_index",
                         "Excerpt Range",
                         "Year",
                         "code",
                         "applied")]

### subset long data to remove non-applied codes
excerpts <- subset(excerpts,
                   applied == 1)

### rename "Index Number" as "Index.Number"
excerpts <- excerpts %>%
  dplyr::rename("Index.Number" = "Index Number",
                "excerpt_range" = "Excerpt Range")

### fuzzy join codes to tool dataset
excerpts_with_codes <- excerpts %>%
  fuzzy_left_join(codes,
                  by = c("code" = "Code"),
                  match_fun = str_detect)

### remove all observations without a code, including metadata variables and "codes not found"
excerpts_with_codes <- subset(excerpts_with_codes,
                              !is.na(Code))

### remove concatenated code variable, relabel data frame
excerpts_with_codes$code <- NULL
excerpts <- excerpts_with_codes

rm(excerpts_with_codes, codes)

### pivot back to wider
excerpts_wide <- pivot_wider(excerpts,
                             names_from = Code.type,
                             values_from = Code)
excerpts <- excerpts_wide

rm(excerpts_wide)

### remove observations from (1) Military Intervention; (2) Fact-finding; or (3) Security Guarantees
excerpts <- excerpts %>%
  subset(!grepl("Military Intervention", Index.Number) &
           !grepl("Security Guarantees", Index.Number) &
           !grepl("Fact-Finding", Index.Number))

#######################

#### store V1 excerpts

#######################

### "website - csv files" working directory
work_dir_csv <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons Learned/Research evidence review/Analysis/V1/Website - CSV files"
setwd(work_dir_csv)

### store cleaned excerpts from V1
write.csv(excerpts, "excerpts_for_analysis_1.csv")

### store cleaned excerpts from all years (only one year for V1)
write.csv(excerpts, "excerpts_for_analysis_all_years.csv")

### reset working directory
work_dir <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/V1"
setwd(work_dir)

######################

### overall effects

######################

### fill in article-level observations at the excerpt level
gen_effects <- excerpts %>%
  group_by(Index.Number) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical, .direction = "up")

### reset Quant as "Multivariate regression" if not Simple Probability
gen_effects <- gen_effects %>%
  mutate(Quant = ifelse(!is.na(Quant) & Quant != "Simple Probability", "Multivariate regression", Quant))

# subset to overall effects observations
gen_effects <- subset(gen_effects, !is.na(gen_effects$`Overall effects`))

### remove excerpt-level information, deduplicate
gen_effects <- gen_effects %>%
  dplyr::select(-c(excerpt_index, excerpt_range)) %>%
  distinct()

# relabel "No effect" and "Mixed effect" as "No or mixed effect"
gen_effects <- gen_effects %>%
  mutate(overall = ifelse(`Overall effects` == "Mixed effect on mass atrocities or CROs" |
                            `Overall effects` == "No measurable effect on mass atrocities or CROs",
                          "No or mixed effects", `Overall effects`)) %>%
  dplyr::select(-`Overall effects`)

###############

### count (average)

###############

### quantity
gen_effects$applied <- as.numeric(gen_effects$applied)
gen_effects <- gen_effects %>%
  group_by(Tool, overall) %>%
  add_tally(name = "count")

### quantity, separate out adverse consequences
gen_effects <- gen_effects %>%
  group_by(Tool) %>%
  mutate(adverse_conseq = ifelse(Outcome == "Adverse consequences", 1, 0)) %>%
  group_by(Tool, overall) %>%
  add_tally(adverse_conseq, name = "count_adverse") %>%
  mutate(count_without_adverse = count - count_adverse) %>%
  dplyr::select(-adverse_conseq)

####################################

### [Half weighted] Number of average-effect findings about MA vs. CRO

####################################

### calculate number of findings related to MA vs. CRO, by Overall and effect
gen_effects <- gen_effects %>%
  mutate(ma_focus_increase_average = ifelse(Outcome == "Mass atrocities" & grepl("Increases", overall), 1, 0),
         ma_focus_decrease_average = ifelse(Outcome == "Mass atrocities" & grepl("Decreases", overall), 1, 0),
         ma_focus_no_mixed_average = ifelse(Outcome == "Mass atrocities" & grepl("mixed", overall), 1, 0),
         cro_focus_increase_average = ifelse(Outcome != "Mass atrocities" & grepl("Increases", overall), 1, 0),
         cro_focus_decrease_average = ifelse(Outcome != "Mass atrocities" & grepl("Decreases", overall), 1, 0),
         cro_focus_no_mixed_average = ifelse(Outcome != "Mass atrocities" & grepl("mixed", overall), 1, 0)) %>%
  mutate(outcome_type = ifelse(Outcome == "Mass atrocities", 1, 0)) %>%
  group_by(Tool, overall, outcome_type) %>%
  add_tally(ma_focus_increase_average, name = "ma_focus_increase_count_average") %>%
  add_tally(ma_focus_decrease_average, name = "ma_focus_decrease_count_average") %>%
  add_tally(ma_focus_no_mixed_average, name = "ma_focus_no_mixed_count_average") %>%
  add_tally(cro_focus_increase_average, name = "cro_focus_increase_count_average") %>%
  add_tally(cro_focus_decrease_average, name = "cro_focus_decrease_count_average") %>%
  add_tally(cro_focus_no_mixed_average, name = "cro_focus_no_mixed_count_average") %>%
  ungroup() %>%
  dplyr::select(-outcome_type)

### replace 0s with NAs
gen_effects <- gen_effects %>%
  mutate(ma_focus_increase_count_average = ifelse(ma_focus_increase_count_average == 0, NA, ma_focus_increase_count_average), 
         ma_focus_decrease_count_average = ifelse(ma_focus_decrease_count_average == 0, NA, ma_focus_decrease_count_average), 
         ma_focus_no_mixed_count_average = ifelse(ma_focus_no_mixed_count_average == 0, NA, ma_focus_no_mixed_count_average),
         cro_focus_increase_count_average = ifelse(cro_focus_increase_count_average == 0, NA, cro_focus_increase_count_average), 
         cro_focus_decrease_count_average = ifelse(cro_focus_decrease_count_average == 0, NA, cro_focus_decrease_count_average), 
         cro_focus_no_mixed_count_average = ifelse(cro_focus_no_mixed_count_average == 0, NA, cro_focus_no_mixed_count_average))

### fill in _count NA values
gen_effects <- gen_effects %>%
  group_by(Tool, overall) %>%
  fill(ma_focus_increase_count_average, ma_focus_decrease_count_average, ma_focus_no_mixed_count_average, 
       cro_focus_increase_count_average, cro_focus_decrease_count_average, cro_focus_no_mixed_count_average) %>%
  fill(ma_focus_increase_count_average, ma_focus_decrease_count_average, ma_focus_no_mixed_count_average, 
       cro_focus_increase_count_average, cro_focus_decrease_count_average, cro_focus_no_mixed_count_average, 
       .direction = "up") %>%
  distinct()

### replace NAs with 0s
gen_effects <- gen_effects %>%
  mutate(ma_focus_increase_count_average = ifelse(is.na(ma_focus_increase_count_average), 0, ma_focus_increase_count_average), 
         ma_focus_decrease_count_average = ifelse(is.na(ma_focus_decrease_count_average), 0, ma_focus_decrease_count_average), 
         ma_focus_no_mixed_count_average = ifelse(is.na(ma_focus_no_mixed_count_average), 0, ma_focus_no_mixed_count_average),
         cro_focus_increase_count_average = ifelse(is.na(cro_focus_increase_count_average), 0, cro_focus_increase_count_average), 
         cro_focus_decrease_count_average = ifelse(is.na(cro_focus_decrease_count_average), 0, cro_focus_decrease_count_average), 
         cro_focus_no_mixed_count_average = ifelse(is.na(cro_focus_no_mixed_count_average), 0, cro_focus_no_mixed_count_average))

####################################

### [Half weighted] Vote count for SoE for average effects

####################################

### create vote score for increase and decrease findings
gen_effects <- gen_effects %>%
  mutate(vote_average = ifelse(grepl("Decreases", overall) & Outcome == "Mass atrocities", 1,
                               ifelse(grepl("Decreases", overall) & Outcome != "Mass atrocities", 0.5,
                                      ifelse(grepl("Increases", overall) & Outcome == "Mass atrocities", -1,
                                             ifelse(grepl("Increases", overall) & Outcome != "Mass atrocities", -0.5,
                                                    NA)))))

### preliminary sum of vote score, for calculating mixed effects
gen_effects <- gen_effects %>%
  group_by(Tool) %>%
  add_tally(vote_average, name = "temp_vote_count_average")

### create vote score for no or mixed effects
gen_effects <- gen_effects %>%
  mutate(vote_average = ifelse(temp_vote_count_average == 0 & grepl("mixed", overall), 0,
                               ifelse(temp_vote_count_average > 0 & grepl("mixed", overall) & Outcome == "Mass atrocities", -0.5,
                                      ifelse(temp_vote_count_average > 0 & grepl("mixed", overall) & Outcome != "Mass atrocities", -0.25,
                                             ifelse(temp_vote_count_average < 0 & grepl("mixed", overall) & Outcome == "Mass atrocities", 0.5,
                                                    ifelse(temp_vote_count_average < 0 & grepl("mixed", overall) & Outcome != "Mass atrocities", 0.25,
                                                           vote_average))))))

### vote count
gen_effects <- gen_effects %>%
  add_tally(vote_average, name = "vote_count_average") %>%
  mutate(vote_count_average = sum(vote_average)) %>%
  mutate(vote_count_average = ifelse(vote_count_average > 0 & temp_vote_count_average < 0, 0,
                                     ifelse(vote_count_average < 0 & temp_vote_count_average > 0, 0,
                                            vote_count_average))) %>%
  dplyr::select(-temp_vote_count_average)

#######################

### record gen_effects

#######################

### revise "Conflict_outcome" in outcome to read "Conflict"
gen_effects <- gen_effects %>%
  mutate(Outcome = ifelse(Outcome == "Conflict_outcome", "Conflict", Outcome))

### add "average" to "effects_type" variable, remove extraneous variables
gen_effects <- gen_effects %>%
  mutate(effects_type = rep("Average")) %>%
  ungroup() %>%
  dplyr::select(-c(applied, `Factor effects`, CD)) %>%
  relocate(effects_type) %>%
  relocate(Gender, Publication, Empirical, Qual, Quant, Method, 
           .after = Year)

#####################

### C&D factors

#####################

### create strength of evidence dataframe
evidence <- excerpts %>%
  dplyr::select(-c(excerpt_index, excerpt_range)) %>%
  group_by(Index.Number) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical, .direction = "up") %>%
  distinct()

### reset Quant as "Multivariate regression" if not Simple Probability
evidence <- evidence %>%
  mutate(Quant = ifelse(!is.na(Quant) & Quant != "Simple Probability", "Multivariate regression", Quant))

# subset to factor effects observations
evidence <- subset(evidence, !is.na(evidence$`Factor effects`))

### convert "applied" variable to numeric format
evidence$applied <- as.numeric(evidence$applied)

### remove rows with CD values with NA (former codes not found)
evidence <- evidence %>%
  subset(!is.na(CD))

#####################

### count (C&D)

#####################

### quantity, aggregated by Tool + C&D factor
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  add_tally(applied, name = "count")

### quantity, separate out adverse consequences
evidence <- evidence %>%
  mutate(adverse_conseq = ifelse(Outcome == "Adverse consequences", 1, 0)) %>%
  group_by(Tool, CD) %>%
  add_tally(adverse_conseq, name = "count_adverse") %>%
  mutate(count_without_adverse = count - count_adverse) %>%
  dplyr::select(-adverse_conseq)

#####################

### distribution of findings (C&D)

#####################

### separate dataframe for article_index (to be merged with factors below after calculating vote_count)
article_level <- evidence %>%
  dplyr::select(Index.Number, Tool, Year, CD, Outcome, `Factor effects`,
                Gender, Publication, Empirical, Qual, Quant, Method) 

### change factor effects variable name + conflict outcome label
article_level <- article_level %>%
  dplyr::rename(Factor_effect = `Factor effects`) %>%
  mutate(Outcome = ifelse(Outcome == "Conflict_outcome", "Conflict", Outcome))

# count number of empirical findings about the effect of the CD factor on each outcome
evidence <- evidence %>%
  group_by(Tool, CD, `Factor effects`) %>%
  add_tally(name = "count_effect")

# compute proportion of findings that align with the observed finding
evidence <- evidence %>%
  mutate(prop_findings = count_effect / count)

# compute the largest proportion of findings that align with the main finding
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  mutate(max_proportion = max(prop_findings))

### identify greater, lesser, and no or mixed findings
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  mutate(greater_findings = ifelse(grepl("greater", `Factor effects`), count_effect, NA),
         lesser_findings = ifelse(grepl("lesser", `Factor effects`), count_effect, NA),
         no_mixed_findings = ifelse(grepl("no or mixed effects", `Factor effects`), count_effect, NA)) %>%
  fill(greater_findings, lesser_findings, no_mixed_findings) %>%
  fill(greater_findings, lesser_findings, no_mixed_findings, .direction = "up")

# compute ratio of findings that have diametrically opposed (i.e., lesser vs. greater) conclusions
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  mutate(prop_opposing = ifelse(greater_findings == 0 | lesser_findings == 0, 0,
                                ifelse(greater_findings > lesser_findings, greater_findings / (greater_findings + lesser_findings),
                                       ifelse(lesser_findings > greater_findings, lesser_findings / (greater_findings + lesser_findings),
                                              ifelse(greater_findings == lesser_findings, 0.5, NA))))) %>%
  mutate(greater_findings = ifelse(is.na(greater_findings), 0, greater_findings),
         lesser_findings = ifelse(is.na(lesser_findings), 0, lesser_findings), 
         no_mixed_findings = ifelse(is.na(no_mixed_findings), 0, no_mixed_findings))

### creating opposing_findings measure
evidence <- evidence %>%
  mutate(opposing_findings = ifelse(greater_findings >= 1 & lesser_findings >= 1,
                                    1, 0))

####################################

### [Half weighted] Number of factor findings about MA vs. CRO

####################################

### calculate number of findings related to MA vs. CRO, by factor and effect
evidence <- evidence %>%
  mutate(ma_focus_greater_factor = ifelse(Outcome == "Mass atrocities" & grepl("greater", `Factor effects`), 1, 0),
         ma_focus_lesser_factor = ifelse(Outcome == "Mass atrocities" & grepl("lesser", `Factor effects`), 1, 0),
         ma_focus_no_mixed_factor = ifelse(Outcome == "Mass atrocities" & grepl("mixed", `Factor effects`), 1, 0),
         cro_focus_greater_factor = ifelse(Outcome != "Mass atrocities" & grepl("greater", `Factor effects`), 1, 0),
         cro_focus_lesser_factor = ifelse(Outcome != "Mass atrocities" & grepl("lesser", `Factor effects`), 1, 0),
         cro_focus_no_mixed_factor = ifelse(Outcome != "Mass atrocities" & grepl("mixed", `Factor effects`), 1, 0)) %>%
  mutate(outcome_type = ifelse(Outcome == "Mass atrocities", 1, 0)) %>%
  group_by(Tool, CD, `Factor effects`, outcome_type) %>%
  add_tally(ma_focus_greater_factor, name = "ma_focus_greater_count_factor") %>%
  add_tally(ma_focus_lesser_factor, name = "ma_focus_lesser_count_factor") %>%
  add_tally(ma_focus_no_mixed_factor, name = "ma_focus_no_mixed_count_factor") %>%
  add_tally(cro_focus_greater_factor, name = "cro_focus_greater_count_factor") %>%
  add_tally(cro_focus_lesser_factor, name = "cro_focus_lesser_count_factor") %>%
  add_tally(cro_focus_no_mixed_factor, name = "cro_focus_no_mixed_count_factor") %>%
  ungroup() %>%
  dplyr::select(-outcome_type)

### replace 0s with NAs
evidence <- evidence %>%
  mutate(ma_focus_greater_count_factor = ifelse(ma_focus_greater_count_factor == 0, NA, ma_focus_greater_count_factor), 
         ma_focus_lesser_count_factor = ifelse(ma_focus_lesser_count_factor == 0, NA, ma_focus_lesser_count_factor), 
         ma_focus_no_mixed_count_factor = ifelse(ma_focus_no_mixed_count_factor == 0, NA, ma_focus_no_mixed_count_factor),
         cro_focus_greater_count_factor = ifelse(cro_focus_greater_count_factor == 0, NA, cro_focus_greater_count_factor), 
         cro_focus_lesser_count_factor = ifelse(cro_focus_lesser_count_factor == 0, NA, cro_focus_lesser_count_factor), 
         cro_focus_no_mixed_count_factor = ifelse(cro_focus_no_mixed_count_factor == 0, NA, cro_focus_no_mixed_count_factor))

### fill in _count_factor NA values
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  fill(ma_focus_greater_count_factor, ma_focus_lesser_count_factor, ma_focus_no_mixed_count_factor, 
       cro_focus_greater_count_factor, cro_focus_lesser_count_factor, cro_focus_no_mixed_count_factor) %>%
  fill(ma_focus_greater_count_factor, ma_focus_lesser_count_factor, ma_focus_no_mixed_count_factor, 
       cro_focus_greater_count_factor, cro_focus_lesser_count_factor, cro_focus_no_mixed_count_factor, 
       .direction = "up") %>%
  distinct()

### replace NAs with 0s
evidence <- evidence %>%
  mutate(ma_focus_greater_count_factor = ifelse(is.na(ma_focus_greater_count_factor), 0, ma_focus_greater_count_factor), 
         ma_focus_lesser_count_factor = ifelse(is.na(ma_focus_lesser_count_factor), 0, ma_focus_lesser_count_factor), 
         ma_focus_no_mixed_count_factor = ifelse(is.na(ma_focus_no_mixed_count_factor), 0, ma_focus_no_mixed_count_factor),
         cro_focus_greater_count_factor = ifelse(is.na(cro_focus_greater_count_factor), 0, cro_focus_greater_count_factor), 
         cro_focus_lesser_count_factor = ifelse(is.na(cro_focus_lesser_count_factor), 0, cro_focus_lesser_count_factor), 
         cro_focus_no_mixed_count_factor = ifelse(is.na(cro_focus_no_mixed_count_factor), 0, cro_focus_no_mixed_count_factor))

####################################

### [Half weighted] Vote count for SoE

####################################

### create vote score for greater and lesser findings
evidence <- evidence %>%
  mutate(vote_factor = ifelse(grepl("greater", `Factor effects`) & Outcome == "Mass atrocities", 1,
                              ifelse(grepl("greater", `Factor effects`) & Outcome != "Mass atrocities", 0.5,
                                     ifelse(grepl("lesser", `Factor effects`) & Outcome == "Mass atrocities", -1,
                                            ifelse(grepl("lesser", `Factor effects`) & Outcome != "Mass atrocities", -0.5,
                                                   NA)))))

### preliminary sum of vote score, for calculating mixed effects
evidence <- evidence %>%
  group_by(Tool, CD) %>%
  add_tally(vote_factor, name = "temp_vote_count_factor")

### create vote score for no or mixed effects
evidence <- evidence %>%
  mutate(vote_factor = ifelse(temp_vote_count_factor == 0 & grepl("mixed", `Factor effects`), 0,
                              ifelse(temp_vote_count_factor > 0 & grepl("mixed", `Factor effects`) & Outcome == "Mass atrocities", -0.5,
                                     ifelse(temp_vote_count_factor > 0 & grepl("mixed", `Factor effects`) & Outcome != "Mass atrocities", -0.25,
                                            ifelse(temp_vote_count_factor < 0 & grepl("mixed", `Factor effects`) & Outcome == "Mass atrocities", 0.5,
                                                   ifelse(temp_vote_count_factor < 0 & grepl("mixed", `Factor effects`) & Outcome != "Mass atrocities", 0.25,
                                                          vote_factor))))))

### vote count
evidence <- evidence %>%
  add_tally(vote_factor, name = "vote_count_factor") %>%
  mutate(vote_count_factor = ifelse(vote_count_factor > 0 & temp_vote_count_factor < 0, 0,
                                    ifelse(vote_count_factor < 0 & temp_vote_count_factor > 0, 0,
                                           vote_count_factor)))

### rename factor effects label
evidence <- evidence %>%
  dplyr::rename(Factor_effect = `Factor effects`)

#####################

### transform to article-level + full list of factors

#####################

### change "Conflict_outcome" to "Conflict"
evidence <- evidence %>%
  mutate(Outcome = ifelse(Outcome == "Conflict_outcome", "Conflict", Outcome))

### list out outcomes related to finding
evidence <- evidence %>%
  dplyr::group_by(CD, Tool) %>%
  dplyr::mutate(outcomes = paste(sort(unique(Outcome)), collapse = ", "))

### identify list of all factors with outcome focus (for article_level merge)
evidence_article_level <- evidence %>%
  dplyr::select(CD, Tool,
                Outcome, Factor_effect) %>%
  distinct()

### merge full list of factors with article_level dataframe
factors_article_level <- left_join(evidence_article_level, article_level, 
                                   by = c("Tool" = "Tool",
                                          "CD" = "CD",
                                          "Outcome" = "Outcome",
                                          "Factor_effect" = "Factor_effect")) %>%
  relocate(Index.Number, Year) %>%
  distinct()

### add "Contextual or design" to "effects_type" variable
factors_article_level <- factors_article_level %>%
  mutate(effects_type = rep("Contextual or design")) %>%
  relocate(effects_type) %>%
  relocate(Gender, Publication, Empirical, Qual, Quant, Method, 
           .after = Year)

### full_join gen_effects to factors_article_level, creating full_article_level
full_article_level <- gen_effects %>%
  full_join(., factors_article_level,
            by = c("effects_type" = "effects_type",
                   "Index.Number" = "Index.Number",
                   "Tool" = "Tool",
                   "Outcome" = "Outcome",
                   "Year" = "Year",
                   "Gender" = "Gender",
                   "Publication" = "Publication",
                   "Empirical" = "Empirical",
                   "Quant" = "Quant",
                   "Qual" = "Qual",
                   "Method" = "Method"))

### remove "Gender" variable
full_article_level <- full_article_level %>%
  dplyr::select(-Gender)

### move Tool, CD, outcome, and effect to the front of the spreadsheet
full_article_level <- full_article_level %>%
  relocate(effects_type, Index.Number, Year, Tool, CD, Factor_effect, overall)

### identify list of all factors with vote count
evidence <- evidence %>%
  dplyr::select(CD, Tool,
                count, 
                greater_findings, lesser_findings, no_mixed_findings, 
                temp_vote_count_factor, vote_count_factor,
                count_adverse, count_without_adverse,
                ma_focus_greater_count_factor, ma_focus_lesser_count_factor, ma_focus_no_mixed_count_factor,
                cro_focus_greater_count_factor, cro_focus_lesser_count_factor, cro_focus_no_mixed_count_factor,
                outcomes) %>%
  distinct()

##########################################

### Add references data to article-level spreadsheet

##########################################

### load references CSV, subset to references that include "documentName" in the extra column
references <- read.csv("zotero_1.csv", encoding = "UTF-8") %>%
  subset(grepl("documentName", Extra))

### extract list of index names in Extra column, separate those with semicolons
references <- references %>%
  mutate(Extra = str_replace(pattern = "documentName:", 
                                replacement = "",
                                Extra)) %>%
  separate(Extra, c("index_1", "index_2", "index_3"),
           sep = ";") %>%
  mutate(index_1 = trimws(index_1),
         index_2 = trimws(index_2),
         index_3 = trimws(index_3))

### create dataframe of references
references_index <- references %>%
  dplyr::select(index_1, index_2, index_3)

### pivot to longer
references_index <- references_index %>%
  pivot_longer(everything(),
               names_to = "remove",
               values_to = "article_index") %>%
  dplyr::select(-remove) %>%
  subset(!is.na(article_index))

### combine with dataset via cycle of left-joins, rbind together
index_1 <- references_index %>%
  left_join(., references,
            by = c("article_index" = "index_1")) %>%
  dplyr::select(-c(index_2, index_3))

index_2 <- references_index %>%
  left_join(., references,
            by = c("article_index" = "index_2")) %>%
  dplyr::select(-c(index_1, index_3))

index_3 <- references_index %>%
  left_join(., references,
            by = c("article_index" = "index_3")) %>%
  dplyr::select(-c(index_1, index_2))

references_index <- rbind(index_1, index_2, index_3)

### replaces pages mis-formatted as dates
references_index <- references_index %>%
  mutate(Pages = ifelse(grepl("Jan", Pages),
                        str_replace(Pages, "Jan", "1"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Feb", Pages),
                        str_replace(Pages, "Feb", "2"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Mar", Pages),
                        str_replace(Pages, "Mar", "3"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Apr", Pages),
                        str_replace(Pages, "Apr", "4"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("May", Pages),
                        str_replace(Pages, "May", "5"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Jun", Pages),
                        str_replace(Pages, "Jun", "6"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Jul", Pages),
                        str_replace(Pages, "Jul", "7"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Aug", Pages),
                        str_replace(Pages, "Aug", "8"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Sep", Pages),
                        str_replace(Pages, "Sep", "9"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Oct", Pages),
                        str_replace(Pages, "Oct", "10"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Nov", Pages),
                        str_replace(Pages, "Nov", "11"),
                        Pages)) %>%
  mutate(Pages = ifelse(grepl("Dec", Pages),
                        str_replace(Pages, "Dec", "12"),
                        Pages))

### create text string for books
references_index <- references_index %>%
  mutate(full_citation = ifelse(Item.Type == "book" & Author == "",
                                paste0(Editor,
                                       ", ed. ",
                                       "*",
                                       Title,
                                       "*",
                                       " ",
                                       Publisher,
                                       ", ",
                                       Publication.Year,
                                       "."),
                                NA)) %>%
  mutate(full_citation = ifelse(Item.Type == "book" & Editor == "",
                                paste0(Author,
                                       ". ",
                                       "*",
                                       Title,
                                       "*",
                                       " ",
                                       Publisher,
                                       ", ",
                                       Publication.Year,
                                       "."),
                                full_citation)) %>%
  relocate(article_index, full_citation)

### create text string for book chapters
references_index <- references_index %>%
  mutate(full_citation = ifelse(Item.Type == "bookSection" & Editor == "",
                                paste0(Author,
                                       ". ",
                                       "\"",
                                       Title,
                                       "\"",
                                       " in ",
                                       "*",
                                       Publication.Title,
                                       "*",
                                       " ",
                                       Publisher,
                                       ", ",
                                       Publication.Year,
                                       "."),
                                full_citation)) %>%
  mutate(full_citation = ifelse(Item.Type == "bookSection" & Editor != "",
                                paste0(Author,
                                       ". ",
                                       "\"",
                                       Title,
                                       "\"",
                                       " in ",
                                       Editor,
                                       ", ed. ",
                                       "*",
                                       Publication.Title,
                                       "*",
                                       " ",
                                       Publisher,
                                       ", ",
                                       Publication.Year,
                                       "."),
                                full_citation)) %>%
  relocate(article_index, full_citation)

### create text string for journal articles
references_index <- references_index %>%
  mutate(full_citation = ifelse(Item.Type == "journalArticle",
                                paste0(Author,
                                       ". ",
                                       "\"",
                                       Title,
                                       ",\"",
                                       " ",
                                       "*",
                                       Publication.Title,
                                       "*",
                                       ", ",
                                       "vol. ",
                                       Volume,
                                       ", ",
                                       "no. ",
                                       Issue,
                                       " (",
                                       Publication.Year,
                                       "), ",
                                       Pages,
                                       ", ",
                                       "DOI: ",
                                       DOI),
                                full_citation)) %>%
  relocate(article_index, full_citation)

### create text string for reports
references_index <- references_index %>%
  mutate(full_citation = ifelse(Item.Type == "report",
                                paste0(Author,
                                       ". ",
                                       "*",
                                       Title,
                                       "*",
                                       ". ",
                                       Publisher,
                                       ", ",
                                       Publication.Year,
                                       "."),
                                full_citation)) %>%
  relocate(article_index, full_citation)

### download list of citations
write.csv(references_index, "citations_1.csv", fileEncoding = "UTF-8")

### load list of citations - intermediate step
references_index <- read.csv("citations_1.csv", encoding = "UTF-8", check.names = F)

### combine citations with article_level dataset
full_article_level <- full_article_level %>%
  left_join(., references_index[, c("article_index", "full_citation")],
            by = c("Index.Number" = "article_index")) %>%
  distinct()

### total number of studies
total_number_of_studies <- length(unique(full_article_level$full_citation))

### subset peer-reviewed articles
number_of_peer_reviewed_studies <- full_article_level %>%
  dplyr::select(full_citation, Publication) %>%
  subset(Publication == "Peer-reviewed journal article")

### calculate number of unique peer-reviewed articles
number_of_peer_reviewed_studies <- length(unique(number_of_peer_reviewed_studies$full_citation))

#########################################

### merge with analytic guide

#########################################

### rename "evidence" as "factors"
factors <- evidence

### reset working directory
work_dir_ag <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons Learned/Research evidence review/Analysis/V1/Analytic guide"
setwd(work_dir_ag)

### rerun analytic guide code
source("G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons Learned/Research evidence review/Analysis/V1/Analytic guide/analytic_guide_1.R")

### load analytic guide
analytic_guide <- read.csv("analytic_guide_1.csv")

### reset working directory to main analysis folder
work_dir <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/V1"
setwd(work_dir)

### subset analytic guide to tools
guide_tools <- analytic_guide %>%
  subset(Type == "Tool") %>%
  dplyr::select(Name, Dedoose.name, Short.description, Description_tool, Theory.of.change,
                Structural.or.operational, Evidence.review,
                dissuade, degrade, protect, transition,
                military, diplomatic, informational, economic, legal)

### subset analytic guide to factors
guide_factors <- analytic_guide %>%
  subset(Type == "Factor") %>%
  dplyr::select(Name, Dedoose.name, Description, Inverse.factor,
                Inverse.factor.description, Contextual.or.design.factor, Family,
                domestic_context, conflict_dynamics, target_characteristics, implementer_choices,
                implementer_characteristics, international_dynamics, tool_specific)

### left_join by dedoose codes (tools)
factors <- factors %>%
  left_join(., guide_tools,
            by = c("Tool" = "Dedoose.name")) %>%
  dplyr::rename(tool_name = Name)

### left_join by dedoose codes (factors)
factors <- factors %>%
  fuzzy_left_join(., guide_factors,
                  by = c("CD" = "Dedoose.name"),
                  match_fun = str_detect)

### trim whitespace on factor names and Dedoose names
factors <- factors %>%
  mutate(Name = trimws(Name),
         CD = trimws(CD))

################################################

### Merge with practitioner data

################################################

### set wd for practitioner data
work_dir_practitioner <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Practitioner knowledge/Survey/Targeted Sanctions/Analysis"
setwd(work_dir_practitioner)

### load practitioner data, associate with targeted sanctions
practitioner <- read.csv("factors_factor_level.csv") %>%
  mutate(Tool = rep("Targeted sanctions_tool"),
         tool_name = rep("Targeted sanctions")) %>%
  dplyr::select(-c(X, num_id, Type))

### full_join practitioner data to master factors dataset
factors <- factors %>%
  full_join(., practitioner,
            by = c("Name" = "Name",
                   "CD" = "Dedoose.name",
                   "Description" = "Description",
                   "Inverse.factor" = "Inverse.factor",
                   "Inverse.factor.description" = "Inverse.factor.description",
                   "Contextual.or.design.factor" = "Contextual.or.design.factor",
                   "Family" = "Family",
                   "Tool" = "Tool",
                   "tool_name" = "tool_name"))

### reset working directory
setwd(work_dir)

##############################################

### [half weighted] Invert (absolute value of) strength of evidence for factors with negative SoE / vote count values

##############################################

### identify factors with negative strength of evidence + inverted factors
factors <- factors %>%
  mutate(inversion_with_neg_strength = ifelse(!is.na(Inverse.factor) & temp_vote_count_factor < 0,
                                              1, 0))

### list of inverted factors
inverted_factors <- factors %>%
  subset(inversion_with_neg_strength == 1) %>%
  dplyr::select(Tool, Name, CD,
                Inverse.factor, vote_count_factor, temp_vote_count_factor,
                inversion_with_neg_strength)

### write csv of inverted factors
write.csv(inverted_factors, "inverted_factors 24 January 2025.csv", fileEncoding = "UTF-8")

### invert factors
factors <- factors %>%
  mutate(Name.Final = ifelse(inversion_with_neg_strength == 1, Inverse.factor, Name),
         Description.Final = ifelse(inversion_with_neg_strength == 1, Inverse.factor.description, Description),
         vote_count = ifelse(inversion_with_neg_strength == 1, abs(vote_count_factor), vote_count_factor))

### invert display name for atrocity_justifying_ideology
factors <- factors %>%
  mutate(Name.Final = ifelse(Name == "Ideologically motivated target", Inverse.factor, Name.Final),
         Description.Final = ifelse(Name == "Ideologically motivated target", Inverse.factor.description, Description.Final))

### invert display name for military_activity
factors <- factors %>%
  mutate(Name.Final = ifelse(Name == "Military sanctions", Inverse.factor, Name.Final),
         Description.Final = ifelse(Name == "Military sanctions", Inverse.factor.description, Description.Final))

##################################

### identify whether gen_effects and factors_article_level articles address MA or CROs

##################################

### select gen_effects article identifiers for each tool
gen_effects_outcomes <- full_article_level %>%
  dplyr::select(Index.Number, Year, Tool, 
                ma_focus_decrease_average, ma_focus_increase_average, ma_focus_no_mixed_average,
                cro_focus_decrease_average, cro_focus_increase_average, cro_focus_no_mixed_average,
                count_adverse, count_without_adverse,
                effects_type) %>% 
  dplyr::rename("ma_focus_greater" = "ma_focus_decrease_average",
         "ma_focus_lesser" = "ma_focus_increase_average",
         "ma_focus_no_mixed" = "ma_focus_no_mixed_average",
         "cro_focus_greater" = "cro_focus_decrease_average",
         "cro_focus_lesser" = "cro_focus_increase_average",
         "cro_focus_no_mixed" = "cro_focus_no_mixed_average") %>%
  subset(effects_type == "Average")

### select factors article identifiers for each tool
factors_outcomes <- full_article_level %>%
  subset(effects_type == "Contextual or design") %>%
  dplyr::select(Index.Number, Year, Tool, Outcome, Factor_effect, effects_type) %>%
  mutate(ma_focus_greater = ifelse(Outcome == "Mass atrocities" & grepl("greater", Factor_effect), 1, 0),
         ma_focus_lesser = ifelse(Outcome == "Mass atrocities" & grepl("lesser", Factor_effect), 1, 0),
         ma_focus_no_mixed = ifelse(Outcome == "Mass atrocities" & grepl("mixed", Factor_effect), 1, 0),
         cro_focus_greater = ifelse(Outcome != "Mass atrocities" & grepl("greater", Factor_effect), 1, 0),
         cro_focus_lesser = ifelse(Outcome != "Mass atrocities" & grepl("lesser", Factor_effect), 1, 0),
         cro_focus_no_mixed = ifelse(Outcome != "Mass atrocities" & grepl("mixed", Factor_effect), 1, 0)) %>%
  dplyr::select(-c(Factor_effect, Outcome)) %>%
  relocate(effects_type, .after = last_col()) %>%
  mutate(count_adverse = NA,
         count_without_adverse = NA)

### combine factors and general effects
outcomes <- rbind(gen_effects_outcomes, factors_outcomes)

### create ma_focus and cro_focus variables
outcomes <- outcomes %>%
  mutate(ma_focus = ifelse(ma_focus_greater == 1 | ma_focus_lesser == 1 | ma_focus_no_mixed == 1, 1, 0),
         cro_focus = ifelse(cro_focus_greater == 1 | cro_focus_lesser == 1 | cro_focus_no_mixed == 1, 1, 0))

### remove type and direction, deduplicate
outcomes <- outcomes %>%
  dplyr::select(Index.Number, Tool, ma_focus, cro_focus) %>%
  distinct()

### count number of factor articles for each tool
outcomes <- outcomes %>%
  group_by(Tool) %>%
  add_tally(ma_focus, name = "ma_focus_tool") %>%
  add_tally(cro_focus, name = "cro_focus_tool")

### select variables to remove count data
outcomes <- outcomes %>%
  dplyr::select(Tool, ma_focus_tool, cro_focus_tool) %>%
  distinct()

### save CSV with outcomes information
write.csv(outcomes, "outcomes 24 January 2025.csv", fileEncoding = "UTF-8")

######################################

### Addendum on average effects with adverse consequences

######################################

### select adverse consequences variables
adverse_consequences <- full_article_level %>%
  subset(effects_type == "Average") %>%
  dplyr::select(Tool, overall, count, count_adverse, count_without_adverse) %>%
  distinct()

### save CSV with adverse consequences findings
write.csv(adverse_consequences, "adverse_consequences 24 January 2025.csv", fileEncoding = "UTF-8")

######################################

### Qualitative description of findings

######################################

### research evidence
factors <- factors %>%
  mutate(research_qualitative = ifelse(vote_count > 3, "Stronger",
                                       ifelse(vote_count <= 3 & vote_count > 1, "Moderate",
                                              ifelse(vote_count <= 1 & vote_count >= 0, "Weaker",
                                                     ifelse(vote_count < 0 & vote_count >= -1, "Weaker",
                                                            ifelse(vote_count < -1 & vote_count >= -3, "Moderate",
                                                                   ifelse(vote_count <= -3, "Stronger",
                                                                          NA)))))))

### add "weaker" to targeted sanctions factors with 0 research findings (and some practitioner findings)
factors <- factors %>%
  mutate(research_qualitative = ifelse(Tool == "Targeted sanctions_tool" & is.na(count),
                                       "None available", research_qualitative))

### practitioner evidence
factors <- factors %>%
  mutate(practitioner_qualitative = ifelse(proportion_practitioner > 2/3, "Stronger",
                                           ifelse(proportion_practitioner <= 2/3 & proportion_practitioner > 1/3, "Moderate",
                                                  ifelse(proportion_practitioner <= 1/3, "Weaker",
                                                         NA))))

### add "weaker" to targeted sanctions factors with 0 practitioner findings (and some research findings)
factors <- factors %>%
  mutate(practitioner_qualitative = ifelse(Tool == "Targeted sanctions_tool" & is.na(count_practitioner),
                                           "Weaker", practitioner_qualitative))

#################################

### Change tool names, add tools without a research review

#################################

### add tools from analytic guide, keep ones without an evidence review
factors <- factors %>%
  full_join(., guide_tools,
            by = c("tool_name" = "Name",
                   "Short.description" = "Short.description",
                   "Description_tool" = "Description_tool",
                   "Theory.of.change" = "Theory.of.change",
                   "Structural.or.operational" = "Structural.or.operational",
                   "Evidence.review" = "Evidence.review",
                   "dissuade" = "dissuade",
                   "degrade" = "degrade",
                   "protect" = "protect",
                   "transition" = "transition",
                   "diplomatic" = "diplomatic",
                   "informational" = "informational",
                   "military" = "military",
                   "economic" = "economic",
                   "legal" = "legal")) %>%
  dplyr::rename("Dedoose.name_factor" = "Dedoose.name.x",
                "Dedoose.name_tool" = "Dedoose.name.y")

#################################

### write CSVs

#################################

### add Name.Final for practitioner interview items
factors <- factors %>%
  mutate(Name.Final = ifelse(is.na(Dedoose.name_factor) & is.na(Name.Final), Name, Name.Final),
         Description.Final = ifelse(is.na(Dedoose.name_factor) & is.na(Description.Final), Description, Description.Final))

### [half weighted] relocate factor name to the beginning of the dataset, remove CD variable
factors <- factors %>%
  relocate(Name.Final, tool_name) %>%
  ungroup() %>%
  dplyr::select(-CD)

### factor_level spreadsheet
write.csv(factors, "all_factors 24 January 2025.csv")

### add reference information to article-level spreadsheet
article_level <- article_level %>%
  left_join(., references_index[, c("article_index", "full_citation")],
            by = c("Index.Number" = "article_index")) %>%
  group_by(Index.Number) %>%
  fill(full_citation) %>%
  fill(full_citation, .direction = "up") %>%
  distinct()

### article_level
write.csv(article_level, "all_sources 24 January 2025.csv", fileEncoding = "UTF-8")

#############################################

### download files without version control labels and tool-specific CSVs

#############################################

### set working directory for CSV files
csv_wd <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/V1/Website - CSV files"
setwd(csv_wd)

############################

### 1. Targeted sanctions

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Targeted sanctions_tool") %>%
  write.csv("targeted_sanctions_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Targeted sanctions_tool") %>%
  write.csv("targeted_sanctions_all_sources_final.csv")

############################

### 2. Mediation

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Mediation_tool") %>%
  write.csv("mediation_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Mediation_tool") %>%
  write.csv("mediation_all_sources_final.csv")

############################

### 3. Naming and shaming

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Naming_and_shaming_tool") %>%
  write.csv("naming_and_shaming_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Naming_and_shaming_tool") %>%
  write.csv("naming_and_shaming_all_sources_final.csv")

############################

### 4. Support to NSAGs

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Support to non-state armed groups_tool") %>%
  write.csv("support_to_nsags_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Support to non-state armed groups_tool") %>%
  write.csv("support_to_nsags_all_sources_final.csv")

############################

### 5. Prosecutions

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Prosecutions_tool") %>%
  write.csv("prosecutions_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Prosecutions_tool") %>%
  write.csv("prosecutions_all_sources_final.csv")

###################################################

### 6. Peace operations

###################################################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Peace operations_tool") %>%
  write.csv("peace_operations_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Peace operations_tool") %>%
  write.csv("peace_operations_all_sources_final.csv")

############################

### 7. Amnesties

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Amnesties_tool") %>%
  write.csv("amnesties_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Amnesties_tool") %>%
  write.csv("amnesties_all_sources_final.csv")

############################

### 8. Arms embargoes

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Arms embargoes_tool") %>%
  write.csv("arms_embargoes_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Arms embargoes_tool") %>%
  write.csv("arms_embargoes_all_sources_final.csv")

############################

### 9. Comprehensive sanctions

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Comprehensive economic sanctions_tool") %>%
  write.csv("comprehensive_sanctions_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Comprehensive economic sanctions_tool") %>%
  write.csv("comprehensive_sanctions_all_sources_final.csv")

############################

### 10. Diplomatic sanctions

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Diplomatic sanctions_tool") %>%
  write.csv("diplomatic_sanctions_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Diplomatic sanctions_tool") %>%
  write.csv("diplomatic_sanctions_all_sources_final.csv")

############################

### 11. Foreign assistance

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Development assistance_tool") %>%
  write.csv("development_assistance_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Development assistance_tool") %>%
  write.csv("development_assistance_all_sources_final.csv")

############################

### 12. Security assistance

############################

### factor-level spreadsheet
factors %>%
  subset(Tool == "Security assistance_tool") %>%
  write.csv("security_assistance_factors_final.csv")

### article-level spreadsheet
article_level %>%
  subset(Tool == "Security assistance_tool") %>%
  write.csv("security_assistance_all_sources_final.csv")

#########################

### master spreadsheets

#########################

### factor_level spreadsheet
factors <- factors %>%
  dplyr::select(-Tool)

write.csv(factors, "all_factors_1.csv")

### article_level
write.csv(article_level, "all_sources_1.csv", fileEncoding = "UTF-8")

### CSV with outcomes information
write.csv(outcomes, "outcomes_1.csv", fileEncoding = "UTF-8")
