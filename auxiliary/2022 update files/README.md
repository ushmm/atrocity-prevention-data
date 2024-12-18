# Master analysis file for Tools for Atrocity Prevention project

The following document annotates the R code for "Lessons Learned in Preventing and Responding to Mass Atrocities," a project of the US Holocaust Memorial Museum's Simon-Skjodt Center for the Prevention of Genocide. 

Daniel Solomon is the original author of this code. Please direct inquiries about this analysis to cpg@ushmm.org.

# Introduction

To run the full code from this document, remove the "eval = FALSE" command in the setup chunk.

```{r setup}
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
```

The analysis requires the following packages. Because these packages are regularly updated, I use the *groundhog* package to associate the code with the package versions that were current as of 13 July 2022---the last date on which I updated the below code.

```{r message = FALSE, warning = FALSE}

library(groundhog)

groundhog.library("dplyr", "2022-07-13")
groundhog.library("stringr", "2022-07-13")
groundhog.library("tidyr", "2022-07-13")
groundhog.library("fuzzyjoin", "2022-07-13")
groundhog.library("reshape", "2022-07-13")
groundhog.library("car", "2022-07-13")
```

I stored the relevant project files in a series of local Google Drive folders. I also stored the auxiliary R files in separate local Drive folders. Accordingly, those files may take some editing to correspond to your local file directory setup.

You may download all supporting CSV files from the "Data" folder on this GitHub page. You may download the auxiliary coding files---in particular, the files related to the analytic guide and practitioner interviews---from the "Auxiliary" folder on this GitHub page.

## Guide to files in Data folder

- *all_factors.csv*: The factor-level dataset that results from the code below.

- *all_sources.csv*: The "study-factor" level dataset that results from the code below.

- *code_guide.csv*: The ordered list of researchers responsible for coding each article, which we use to filter the excerpt-level data from Dedoose.

- *data_dictionary.csv*: A guide to the variables in the *all_factors.csv* and *all_sources.csv* datasets.

- *excerpts.csv*: The excerpt-level data from Dedoose.

- *master_codes.csv*: The list of codes that researchers applied to studies in Dedoose.

## Guide to files in Auxiliary folder

- *[for editing] LL analytic guide to coding.docx*: The Word-formatted version of the analytic guide for coding studies. This document contains descriptive information about factors that are associated with the effectiveness of atrocity prevention tools. 

- *[for editing] Practitioner factor guide.docx*: The Word-formatted version of the analytic guide associated with the practitioner interviews.

- *analytic_guide.csv*: The spreadsheet-formatted version of the analytic guide.

- *anaytic_guide.R*: The R code for transforming the Word-formatted version of the analytic guide into a spreadsheet-formatted document.

- *factors_factor_level.csv*: The summary information from our practitioner interviews.

- *practitioner_master.R*: The R code for summarizing the practitioner interview data.

- *practitioner_survey.csv*: The anonymized data from our interviews with experienced practitioners. Note that we have replaced cells that contain identifying information about the interviewees with a "XXX" string. In some cases, these include transcript columns.

- *zotero.csv*: References information for the files in the *all_sources.csv* document. This file is enormous, but deleting irrelevant bibliographic information was inefficient. The file is too large to post on GitHub; please contact us at cpg@ushmm.org if you require access to it.

# Load and subset data

Multiple project researchers generated the original data in Dedoose, a qualitative data collection software. In Dedoose, researchers use a pre-determined set of "codes" to distill the information contained in "excerpts" from the qualitative data files---in our case, the studies that comprise our systematic review. Each study contained multiple excerpts. Although the unit of observation for the Dedoose data is the excerpt, our unit of analysis is the study. 

Below, I detail how I transformed, filtered, and cleaned the Dedoose data for further analysis.

```{r message=FALSE, warning=FALSE}

### load excerpt-level data
excerpts <- read.csv("excerpts.csv", na.strings = "", check.names = F) %>%
  dplyr::select(-309)

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

```

Because multiple project researchers coded each article, we used an ordered list of coders to filter excerpt codes for further analysis. The ordered list for each article is located in the document *code_guide.csv*. We also used the document *master_codes.csv* to ensure that we had downloaded excerpts from the correct list of codes. To filter out excluded codes, we appended these documents to the original excerpts dataset.

```{r message = FALSE, warning = FALSE}

### code_guide with key to correct coders
code_guide <- read.csv("code_guide.csv", na.strings = "")

### master list of codes, remove Category and Sub.category columns
codes <- read.csv("master_codes.csv")
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

```

Because we downloaded the data from Dedoose at the "excerpt" level, I transformed the original excerpts dataset from a "long" format to a "wide," study-level format. As I discuss in the section below, the study-level dataset allowed us to summarize conclusions across studies about the effects of atrocity prevention tools.

```{r message = FALSE, warning = FALSE}

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

```

As we note in the longer description of the project methodology, we collected data about studies in the military intervention literature. However, we observed that a majority of the 44 quantitative studies about military intervention rely on datasets that conflate active combat operations with either (1) peacekeeping operations or (2) security assistance and material support to non-state armed groups. We concluded that we needed more time to sort out the complicated definitional issues associated with this literature, and excluded the military intervention studies from our final review. 

We also collected data about studies in the literature about fact-finding and security guarantees. After beginning to collect data about these studies, however, we concluded that fewer than five studies in each literature met the inclusion criteria that we detail in the methodology report. Accordingly, we excluded studies from each literature from our review.   

```{r message = FALSE, warning = FALSE}

### remove observations from (1) Military Intervention; (2) Fact-finding; or (3) Security Guarantees
excerpts <- excerpts %>%
  subset(!grepl("Military Intervention", Index.Number) &
           !grepl("Security Guarantees", Index.Number) &
           !grepl("Fact-Finding", Index.Number))

```

# Analysis: Overall effects

The first set of information addresses the overall or average effect of an atrocity prevention tool on mass atrocities or closely-related outcomes---i.e., was the tool associated with higher or lower levels of mass atrocities, or did it have no or mixed effects? In this section, I describe the process by which we summarized relevant studies' conclusions about the overall effects of atrocity prevention tools.

```{r message = FALSE, warning = FALSE}

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

```

I calculated the number of studies about the overall effects of each atrocity prevention tool, separated by the direction of the tool's effect. 

I also calculated the separate number of studies about the "adverse consequences" associated with each atrocity prevention tool. Although we assume that these findings are relevant to the consideration of when and how to use these tools most effectively, we expect that policy practitioners view them as a separate category of outcomes from mass atrocities and other closely-related forms of violence.

```{r message = FALSE, warning = FALSE}

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

```

## Strength of evidence

In the longer description of the project methodology, we describe the modified "vote counting" procedure that we used to assess evidence about atrocity prevention tools and the contextual and design factors that are associated with their effectiveness. We also calculated the vote count for the evidence about the overall effects of each tool.

```{r message = FALSE, warning = FALSE}

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

```

Although we do not present the results of the overall effects separately, we include the final results in the *all_sources.csv* file that I export below. 

```{r message = FALSE, warning = FALSE}

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

```

# Analysis: Contextual and design (C&D) factors

The second set of information addresses the contextual or design factors that were associated with greater or lesser effectiveness of the atrocity prevention tool. This information is the central focus of the project's Tools for Atrocity Prevention website. In this section, I describe the process by which we summarized relevant studies' conclusions about the "factor effects" associated with atrocity prevention tools.

```{r message = FALSE, warning = FALSE}

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

```

I calculated the number of studies about the factor effects associated with each tool. As above, I also calculated the separate number of studies about the "adverse consequences" associated with each factor effect. 

```{r message = FALSE, warning = FALSE}

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

```

## Distribution of findings

I calculated the distribution of findings about factor effects---i.e., whether the factor was associated with greater or lesser effectiveness of the prevention tool, or had no or mixed effects. 

```{r message = FALSE, warning = FALSE}

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


```

## Strength of evidence

In the longer description of the project methodology, we describe the modified "vote counting" procedure that we used to assess evidence about atrocity prevention tools and the contextual and design factors that are associated with their effectiveness. 

```{r message = FALSE, warning = FALSE}

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

```

# Finalizing the data

## Core datasets

Our analysis results in two core datasets: (1) a factor-level dataset, which includes summary information about each factor that is associated with the effectiveness of the prevention tool; and (2) a "study-factor" level dataset, which includes information about the methods of data collection and analysis in each study, as well as the conclusions about overall and factor effects in each study.

```{r message = FALSE, warning = FALSE}

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

```

## References

To ensure that researchers can access bibliographic information about each study, we downloaded the citation information for each study using the Zotero reference manager. Readers should note that the Zotero information likely contains typographic errors, both because we did not modify the references data at this stage and because the CSV encoding does not recognize all special characters. 

I combined this reference information with the information in the study-level dataset. 

```{r message = FALSE, warning = FALSE}

##########################################

### Add references data to article-level spreadsheet

##########################################

### load references CSV, subset to references that include "documentName" in the extra column
references <- read.csv("zotero.csv", encoding = "UTF-8") %>%
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
write.csv(references_index, "citations.csv", fileEncoding = "UTF-8")

```

After downloading the list of citations, you should copy the *citations.csv* file and relabel it as *citations_final.csv*. This ensures that you are using the latest version of the citations final.

```{r message = FALSE, warning = FALSE}

### load list of citations
references_index <- read.csv("citations_final.csv", encoding = "UTF-8", check.names = F)

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
  subset(Publication == "Peer reviewed journal article")

### calculate number of unique peer-reviewed articles
number_of_peer_reviewed_studies <- length(unique(number_of_peer_reviewed_studies$full_citation))

```

## Analytic guide

The project's "analytic guide" includes descriptive information about our definitions of factors and the categories into which we sort them (e.g., "domestic context" versus "implementer choices"). I scraped a .docx version of the analytic guide using the *analytic_guide.R* code and combined the analytic-guide information with the information in the factor-level dataset.

```{r message = FALSE, warning = FALSE}

#########################################

### merge with analytic guide

#########################################

### rename "evidence" as "factors"
factors <- evidence

### reset working directory
work_dir_ag <- "G:/My Drive/CPG/Projects/Research/Lessons-Learned/Kyra's Folder/Lessons learned/1. Collecting, Distilling, and Organizing the Knowledge/Empirical Knowledge/4. NEW coding process/Analytic guide"
setwd(work_dir_ag)

### rerun analytic guide code
source("G:/My Drive/CPG/Projects/Research/Lessons-Learned/Kyra's Folder/Lessons learned/1. Collecting, Distilling, and Organizing the Knowledge/Empirical Knowledge/4. NEW coding process/Analytic guide/analytic_guide.R")

### load analytic guide
analytic_guide <- read.csv("analytic_guide.csv")

### reset working directory to main analysis folder
work_dir <- "G:/My Drive/CPG/Projects/Research/Lessons-Learned/Kyra's Folder/Lessons learned/1. Collecting, Distilling, and Organizing the Knowledge/Empirical Knowledge/4. NEW coding process/Analysis"
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

```

## Add practitioner interview data

As we discuss in the project methodology description, a second component of the project involves interviewing experienced practitioners with experience working on select tools---in particular, targeted financial sanctions. I summarized the information from the interviews using the *practitioner_master.R* code and combined the summary information with the information from the factor-level dataset.

```{r message = FALSE, warning = FALSE}

################################################

### Merge with practitioner data

################################################

### set wd for practitioner data
work_dir_practitioner <- "G:/My Drive/CPG/Projects/Research/Lessons-Learned/Kyra's Folder/Lessons learned/1. Collecting, Distilling, and Organizing the Knowledge/Practitioner Knowledge/Survey/Analysis"
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

```

# Web display

The project's central output is the Tools for Atrocity Prevention website, which summarizes the conclusions of our systematic review. Researchers may access the website at https://preventiontools.ushmm.org/, and supporting materials for the rest of the project at https://www.ushmm.org/lessons-learned. In the following sections, we transform the "raw" data in preparation for the web display.

## Inverted factors

For the Tools for Atrocity Prevention website, we "inverted"---reversed the directional effect of---factors that (1) were associated with lesser effectiveness of the prevention tool and (2) had an obvious opposite value. 

```{r message = FALSE, warning = FALSE}

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
write.csv(inverted_factors, "inverted_factors 13 July 2022 8am.csv", fileEncoding = "UTF-8")

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

```

Note: Where the research evidence and practitioner knowledge indicate disagreement about the direction of the effect of a specific factor, we do not display either the research finding or the summary of practitioner perspectives on the Tools for Atrocity Prevention website. However, we keep all data about the research evidence and practitioner knowledge in the raw CSV files. This disagreement only applies to one factor-level finding—namely, the timing of the implementation of targeted sanctions (listed as “Early / late implementation” in the CSV files). We plan to account for this discrepancy in future project updates.

## Outcomes

For the summary information on the website, we calculated the number of findings about mass atrocities versus closely-related outcomes.

```{r message = FALSE, warning = FALSE}

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
write.csv(outcomes, "outcomes 13 July 2022 8am.csv", fileEncoding = "UTF-8")

```

## "Adverse consequences" addendum

For a select number of tools---in particular, comprehensive economic sanctions---"adverse consequences" comprise a large number of findings about the tool's effect on closely-related outcomes. We assume that these findings are relevant to the consideration of when and how to use these tools most effectively. Because these consequences may be distinct from direct outcomes, however, we also draw attention to separate adverse consequences findings from the literature.

``` {r message = FALSE, warning = FALSE}

### select adverse consequences variables
adverse_consequences <- full_article_level %>%
  subset(effects_type == "Average") %>%
  dplyr::select(Tool, overall, count, count_adverse, count_without_adverse) %>%
  distinct()

### save CSV with adverse consequences findings
write.csv(adverse_consequences, "adverse_consequences 13 July 2022 8am.csv", fileEncoding = "UTF-8")

```

## Qualitative description

For the website, we converted the quantitative vote count about each factor to a qualitative range of "Stronger," "Moderate," and "Weaker" evidence.

```{r message = FALSE, warning = FALSE}

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

```

## Tools without a research review

On the website, we also include a list of atrocity prevention tools for which we did not conduct a research review.

```{r message = FALSE, warning = FALSE}

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

```
         
# Downloading data

The final step is downloading the datasets with our analytic results. First, I export the core datasets; second, I export the datasets disaggregated by prevention tool.

## Core datasets

```{r message = FALSE, warning = FALSE}

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
write.csv(factors, "all_factors 13 July 2022 8am.csv")

### article_level
write.csv(article_level, "all_sources 13 July 2022 8am.csv", fileEncoding = "UTF-8")

```

## Disaggregated by prevention tool

```{r message = FALSE, warning = FALSE}

#############################################

### download files without version control labels and tool-specific CSVs

#############################################

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

write.csv(factors, "all_factors.csv")

### article_level
write.csv(article_level, "all_sources.csv", fileEncoding = "UTF-8")

### CSV with outcomes information
write.csv(outcomes, "outcomes.csv", fileEncoding = "UTF-8")

```
