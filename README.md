# Master analysis file for Tools for Atrocity Prevention project

The following document annotates the R code for the analysis underlying the [Tools for Atrocity Prevention](https://preventiontools.ushmm.org/) (TfAP) website, a project of the United States Holocaust Memorial Museum's Simon-Skjodt Center for the Prevention of Genocide. The code applies to the current version of the TfAP website, last updated in April 2025. Prior versions of the code can be found in the following folders: (1) [July 2022](https://github.com/ushmm/atrocity-prevention-data/tree/main/auxiliary/2022%20update%20files); (2) [April 2024](https://github.com/ushmm/atrocity-prevention-data/tree/main/auxiliary/2024%20update%20files). 

The principal differences between this code and the April 2024 version are twofold: (1) this version includes an analysis of interviews with 13 practitioners of peace operations, to complement the analysis of interviews with 15 practitioners of targeted sanctions; and (2) this version includes code that addresses circumstances in which practitioner perspectives about the anticipated effect of specific success factors differs from the prevailing research evidence.

Daniel Solomon is the original author of this code. Please direct inquiries about this analysis to cpg@ushmm.org.

# Introduction

To run the full code from this document, remove the "eval = FALSE" command in the setup chunk.

```{r setup}
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
```

The analysis requires the following packages. Prior versions of this code used the *groundhog* package to associate the code with the package versions that were current as of 13 July 2022, but the *groundhog* approach created cascading errors in subsequent analyses. Accordingly, I've updated the code to the default "library(XXXpackage)" approach. As of July 2025, all packages used in this analysis were regularly updated.

```{r message = FALSE, warning = FALSE}

library(dplyr) ### data wrangling
library(stringr) ### edit strings
library(tidyr) ### data wrangling
library(fuzzyjoin) ### fuzzy text joins
library(reshape) ### reshape datasets
library(ggplot) ### plots

```

I stored the relevant project files in a series of local Google Drive folders. I also stored the auxiliary R files in separate local Drive folders. Accordingly, those files may take some editing to correspond to your local file directory setup.

You may download all supporting CSV files from the "Data" folder on this GitHub page. You may download the auxiliary coding files---in particular, the files related to the analytic guide and practitioner interviews---from the "Auxiliary" folder on this GitHub page.

## Guide to files in Data folder

- *all_factors.csv*: The factor-level dataset that results from the code below.

- *all_sources.csv*: The "study-factor" level dataset that results from the code below.

- *code_guide_2023.csv*: The list of researchers responsible for coding each article, which we use to filter the excerpt-level data from Dedoose.

- *data_dictionary_2023.csv*: A guide to the variables in the *all_factors.csv* and *all_sources.csv* datasets.

- *excerpts_2023.csv*: The excerpt-level data from Dedoose, the qualitative coding software that we use for this project. This is the cumulative data from Dedoose, combining excerpts from the most recent studies and studies that we included in previous project updates.

- *master_codes_2023.csv*: The list of codes that researchers applied to studies in Dedoose.

## Guide to files in Auxiliary folder

- *analytic_guide_2023.docx*: The Word-formatted version of the analytic guide for coding studies. This document contains descriptive information about factors that are associated with the effectiveness of atrocity prevention tools. 

- *[for editing] Practitioner factor guide.docx*: The Word-formatted version of the analytic guide associated with the practitioner interviews.

- *analytic_guide.csv*: The spreadsheet-formatted version of the analytic guide.

- *anaytic_guide.R*: The R code for transforming the Word-formatted version of the analytic guide into a spreadsheet-formatted document.

- Original interview data, based on the practitioner interview transcripts:

  - *[ts] practitioner_survey.csv*: The anonymized data from our interviews with experienced practitioners about targeted sanctions. Note that we have replaced cells that contain identifying information about the interviewees with a "XXX" string. In some cases, these include transcript columns. In the R code *practitioner_master*, this file is called "practitioner_survey.csv." I've changed the name here for clarity.
  - *[po] practitioner_survey.csv*: The anonymized data from our interviews with experienced practitioners about targeted sanctions. In the R code *peace_ops_summary*, this file is called "[with negative coding] Coding LL CF_DF by Interview - Sheet1.csv." I've changed the name here for clarity and brevity.

- Factor-level data, based on the practitioner interviews:

  - Targeted sanctinos: *[ts] factors_factor_level.csv*: The summary information from our practitioner interviews about targeted sanctions. Originally, these data were stored in a separate working directory from the peace operations data. In the R codes *practitioner_master* and *master_analysis_2023*, this file is called "factors_factor_level.csv." I've changed the name here for clarity.
  - Peace operations: *[po] factors_factor_level.csv*: The summary information from our practitioner interviews about targeted sanctions. Originally, these data were stored in a separate working directory from the targeted sanctions data. In the R codes *peace_ops_summary* and *master_analysis_2023*, this file is called "factors_factor_level.csv." I've changed the name here for clarity.

- R codes for the practitioner interviews:

  - Targeted sanctions: *practitioner_master.R* - The R code for summarizing the practitioner interview data about targeted sanctions.
  - Peace operations: *peace_ops_summary.R* - The R code for summarizing the practitioner interview data about peace operations.

- *zotero.csv*: References information for the files in the *all_sources.csv* document. Previous versions of this file were too large to post on GitHub; we've since consolidated the Zotero database.

# Load and subset data

Multiple project researchers generated the original data in Dedoose. In Dedoose, researchers use a pre-determined set of "codes" to distill the information contained in "excerpts" from the qualitative data files---in our case, the studies that comprise our systematic review. Each study contained multiple excerpts. Although the unit of observation for the Dedoose data is the excerpt, our unit of analysis is the study. 

Below, I detail how I transformed, filtered, and cleaned the Dedoose data for further analysis.

```{r message=FALSE, warning=FALSE}

### load excerpt-level data - download only current year from Dedoose, need to remove erroneous empty last column
excerpts <- read.csv("excerpts_2023.csv", na.strings = "", check.names = F) %>%
  dplyr::select(-330)

### change variable names with spaces to periods
names(excerpts) <- gsub(x = names(excerpts),
                        pattern = " ",
                        replacement = ".") 

### change "True" and "False" to "1" and "0", respectively
excerpts[excerpts == "True"] <- 1
excerpts[excerpts == "False"] <- 0

### subset to remove NA rows [should only be -1 row]
excerpts <- excerpts %>%
  subset(!is.na(Excerpt.Creator))

```

Because multiple project researchers coded each article, we used an ordered list of coders to filter excerpt codes for further analysis. The ordered list for each article is located in the document *code_guide_2023.csv*. We also used the document *master_codes_2023.csv* to ensure that we had downloaded excerpts from the correct list of codes. To filter out excluded codes, we appended these documents to the original excerpts dataset.

```{r message = FALSE, warning = FALSE}

### code_guide with key to codes
code_guide <- read.csv("code_guide_2023.csv", na.strings = "")

### master list of codes
codes <- read.csv("master_codes_2023.csv")

### identify correct codes

### left join tool dataframe with list of correct coders
excerpts <- excerpts %>%
  left_join(., code_guide[, c("index_number", "excluded")],
            by = c("Index.Number" = "index_number"))
  
### identify correct coders
excerpts <- excerpts %>%
  mutate(correct_coder = ifelse(Excerpt.Creator == "dsolomon_ushmm", 1, 0))

### subset to remove incorrect coders
excerpts <- excerpts %>% 
  subset(correct_coder == 1)

### subset to remove excluded articles
excerpts <- excerpts %>%
  subset(excluded == 0)

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
excerpts$excerpt_index <- paste0("2023_", seq(1:nrow(excerpts)))

### pivot wider to longer
excerpts_long <- excerpts %>%
  pivot_longer(cols = starts_with("Code:"),
               names_to = "code",
               names_prefix = "Code:",
               values_to = "applied")

### rename variables and remove extraneous Excerpt Creator, Codes.Applied.Combined, Title, Author, Year, X variables
excerpts <- excerpts_long %>%
  dplyr::rename(Index.Number = `Index Number`,
                excerpt_range = `Excerpt Range`) %>%
  dplyr::select(Index.Number, excerpt_index, excerpt_range, Year, code, applied)

### subset long data to remove non-applied codes
excerpts <- excerpts %>%
  subset(applied == 1)

### fuzzy join codes to tool dataset
excerpts_with_codes <- excerpts %>%
  fuzzy_left_join(codes,
                  by = c("code" = "Code"),
                  match_fun = str_detect)

### quality check: remove all observations without a code, including metadata variables and "codes not found"
### there should be none of these, since all "codes not found" will have been resolved
excerpts_with_codes <- subset(excerpts_with_codes,
                              !is.na(Code))

### remove concatenated code variable, relabel data frame
excerpts_with_codes$code <- NULL
excerpts <- excerpts_with_codes

rm(codes, excerpts_long)

### pivot back to wider (quality check: should only identify unique values)
excerpts_wide <- pivot_wider(excerpts,
                             names_from = Code.type,
                             values_from = Code)

### re-store excerpts
excerpts <- excerpts_wide

rm(excerpts_wide, excerpts_with_codes)

```

# Analysis: Overall effects

The first set of information addresses the overall or average effect of an atrocity prevention tool on mass atrocities or closely-related outcomes---i.e., was the tool associated with higher or lower levels of mass atrocities, or did it have no or mixed effects? In this section, I describe the process by which we summarized relevant studies' conclusions about the overall effects of atrocity prevention tools.

```{r message = FALSE, warning = FALSE}

### fill in article-level observations at the excerpt level
gen_effects <- excerpts %>%
  group_by(Index.Number) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical, .direction = "up")

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

In the [longer description](https://www.ushmm.org/genocide-prevention/simon-skjodt-center/work/lessons-learned/methodology) of the project methodology on the Simon-Skjodt Center's project site, we describe the modified "vote counting" procedure that we used to assess evidence about the effects of atrocity prevention tools and the contextual and design factors that are associated with their effectiveness.

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

The second set of information addresses the contextual or design factors that were associated with the effectiveness of atrocity prevention tools. This information is the central focus of the project's TfAP website. In this section, I describe the process by which we summarized relevant studies' conclusions about the "factor effects" associated with atrocity prevention tools.

```{r message = FALSE, warning = FALSE}

### create strength of evidence dataframe
evidence <- excerpts %>%
  dplyr::select(-c(excerpt_index, excerpt_range)) %>%
  group_by(Index.Number) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical) %>%
  fill(Publication, Gender, Quant, Qual, Method, Empirical, .direction = "up") %>%
  distinct()

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

```

## Strength of evidence

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
                Outcome, Factor_effect,
                count, count_adverse, count_without_adverse, 
                greater_findings, lesser_findings, no_mixed_findings,
                ma_focus_greater_factor,
                ma_focus_lesser_factor,
                ma_focus_no_mixed_factor,
                cro_focus_greater_factor,
                cro_focus_lesser_factor,
                cro_focus_no_mixed_factor) %>%
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
                   "Method" = "Method",
                   "count" = "count",
                   "count_adverse" = "count_adverse",
                   "count_without_adverse" = "count_without_adverse"))

### remove "Gender" variable - we collected information about author gender but it is not central to our analysis
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
references <- read.csv("zotero_2023.csv", encoding = "UTF-8") %>%
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

### combine with dataset via cycle of left-joins, rbind together, omit missing observations
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

references_index <- rbind(index_1, index_2, index_3) %>%
  subset(!is.na(Key))

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

### create text string for manuscripts
references_index <- references_index %>%
  mutate(full_citation = ifelse(Item.Type == "manuscript",
                                paste0(Author,
                                       ". ",
                                       "\"",
                                       Title,
                                       "\".",
                                       "Working paper",
                                       ", ",
                                       Publication.Year,
                                       "."),
                                full_citation)) %>%
  relocate(article_index, full_citation)

### download list of citations
write.csv(references_index, "citations_2023.csv", fileEncoding = "UTF-8")

### combine citations with article_level dataset, replace (Dedoose) Year with Publication.Year and rename as Year
full_article_level <- full_article_level %>%
  left_join(., references_index[, c("article_index", "full_citation", "Publication.Year")],
            by = c("Index.Number" = "article_index")) %>%
  group_by(Index.Number) %>%
  fill(full_citation) %>%
  fill(full_citation, .direction = "up") %>%
  dplyr::select(-Year) %>%
  dplyr::rename(Year = Publication.Year) %>%
  distinct()

### total number of studies
total_number_of_studies <- length(unique(full_article_level$full_citation))

### total number of studies examining multiple tools
total_number_multiple_tools <- full_article_level %>%
  dplyr::select(Index.Number, full_citation) %>%
  group_by(full_citation) %>%
  distinct() %>%
  mutate(sum = ifelse(n() > 1, 1, 0)) %>%
  ungroup() %>%
  dplyr::select(sum)
total_number_multiple_tools <- sum(total_number_multiple_tools$sum)

### subset peer-reviewed articles
number_of_peer_reviewed_studies <- full_article_level %>%
  dplyr::select(full_citation, Publication) %>%
  subset(Publication == "Peer-reviewed journal article")

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
work_dir_ag_2023 <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/2023/Analytic guide"
setwd(work_dir_ag_2023)

### rerun analytic guide code
source("G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Research evidence review/Analysis/2023/Analytic guide/analytic_guide_2023.R")

### load analytic guide
analytic_guide <- read.csv("analytic_guide_2023.csv")

### reset working directory to main analysis folder
setwd(work_dir_2023)

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

A second component of the project involves interviewing experienced practitioners with experience working on select tools---as of the 2025 release, this group of practitioners includes those who work on (1) targeted financial sanctions and (2) peace operations. I summarized the information from the interviews using the *practitioner_master* and *peace_ops_summary* codes, respectively, and combined the summary information with the information from the factor-level dataset.

### Targeted sanctions

```{r message = FALSE, warning = FALSE}

################################################

### Merge with practitioner data - targeted sanctions

################################################

### set wd for practitioner data 
### [need to re-run manually if there are any updates to practitioner data or code categories]
work_dir_practitioner_targeted_sanctions <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Practitioner knowledge/Survey/Targeted Sanctions/Analysis"
setwd(work_dir_practitioner_targeted_sanctions)

### load practitioner data, associate with targeted sanctions
practitioner_targeted_sanctions <- read.csv("factors_factor_level.csv") %>%
  mutate(Tool = rep("Targeted sanctions_tool"),
         tool_name = rep("Targeted sanctions")) %>%
  dplyr::select(-c(X, num_id, Type))

### full_join practitioner data to master factors dataset
factors_targeted_sanctions <- factors %>%
  subset(Tool == "Targeted sanctions_tool") %>%
  full_join(., practitioner_targeted_sanctions,
            by = c("Name" = "Name",
                   "CD" = "Dedoose.name",
                   "Description" = "Description",
                   "Inverse.factor" = "Inverse.factor",
                   "Inverse.factor.description" = "Inverse.factor.description",
                   "Contextual.or.design.factor" = "Contextual.or.design.factor",
                   "Family" = "Family",
                   "Tool" = "Tool",
                   "tool_name" = "tool_name"))

### fill in, deduplicate
factors_targeted_sanctions <- factors_targeted_sanctions %>%
  group_by(CD) %>%
  fill(-CD) %>%
  fill(-CD, .direction = "up") %>%
  distinct()

### remove targeted sanctions from master factors
factors_master <- factors %>%
  subset(Tool != "Targeted sanctions_tool")

### combine factors_master and factors_targeted_sanctions
factors <- rbind(factors_master, factors_targeted_sanctions)

### reset working directory
setwd(work_dir_2023)

```

### Peace operations

``` {r message = FALSE, warning = FALSE}

### set wd for practitioner data 
### [need to re-run manually if there are any updates to practitioner data or code categories]
work_dir_practitioner_peace_ops <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Practitioner knowledge/Survey/Peace Operations/Analysis"
setwd(work_dir_practitioner_peace_ops)

### load practitioner data, associate with peace operations
practitioner_peace_ops <- read.csv("factors_factor_level.csv") %>%
  mutate(Tool = rep("Peace operations_tool"),
         tool_name = rep("Peace operations")) %>%
  dplyr::select(-X)

### full_join practitioner data to master factors dataset
factors_peace_ops <- factors %>%
  subset(Tool == "Peace operations_tool") %>%
  full_join(., practitioner_peace_ops,
            by = c("Name" = "short_new_description",
                   "CD" = "Dedoose.name",
                   "Description" = "Description",
                   "Inverse.factor" = "Inverse.factor",
                   "Inverse.factor.description" = "Inverse.factor.description",
                   "Contextual.or.design.factor" = "Contextual.or.design.factor",
                   "Family" = "Family",
                   "Tool" = "Tool",
                   "tool_name" = "tool_name",
                   "count_practitioner" = "count_practitioner",
                   "number_practitioners" = "number_practitioners",
                   "proportion_practitioner" = "proportion_practitioner",
                   "greater_practitioner" = "greater_practitioner",
                   "lesser_practitioner" = "lesser_practitioner",
                   "no_mixed_practitioner" = "no_mixed_practitioner"))

### fill in, deduplicate
factors_peace_ops <- factors_peace_ops %>%
  group_by(Name) %>%
  fill(-c(Name)) %>%
  fill(-c(Name), .direction = "up") %>%
  distinct() %>%
  ungroup() %>%
  group_by(tool_name) %>%
  fill(Short.description, Description_tool, Theory.of.change,
       Structural.or.operational, Evidence.review, Strategies.the.tool.can.support, DIMEL) %>%
  fill(Short.description, Description_tool, Theory.of.change,
       Structural.or.operational, Evidence.review, Strategies.the.tool.can.support, DIMEL, .direction = "up") %>%
  distinct()
  
### remove peace ops from master factors
factors_master <- factors %>%
  subset(Tool != "Peace operations_tool")

### combine factors_master and factors_peace_ops
factors <- rbind(factors_master, factors_peace_ops)

### reset working directory
setwd(work_dir_2023)

``` 

# Web display

The project's central output is the Tools for Atrocity Prevention website, which summarizes the conclusions of our systematic review. Researchers may access the website at https://preventiontools.ushmm.org/, and supporting materials for the rest of the project at https://www.ushmm.org/lessons-learned. In the following sections, we transform the "raw" data in preparation for the web display.

## Inverted factors

For the Tools for Atrocity Prevention website, we "inverted"---reversed the directional effect of---factors that (1) were associated with lesser effectiveness of the prevention tool and (2) had an obvious opposite value. 

```{r message = FALSE, warning = FALSE}

##############################################

### Invert (absolute value of) strength of evidence for factors with negative SoE / vote count values

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
write.csv(inverted_factors, "inverted_factors 2 June 2025.csv", fileEncoding = "UTF-8")

### invert factors
factors <- factors %>%
  mutate(Name.Final_research = ifelse(is.na(count), NA,
                                      ifelse(inversion_with_neg_strength == 1, Inverse.factor, Name)),
         Description.Final_research = ifelse(is.na(count), NA,
                                             ifelse(inversion_with_neg_strength == 1, Inverse.factor.description, Description)),
         vote_count_factor = ifelse(is.na(count), NA,
                                    ifelse(inversion_with_neg_strength == 1, abs(vote_count_factor), vote_count_factor)))

### invert factors for practitioner name
factors <- factors %>%
  mutate(Name.Final_practitioner = ifelse(is.na(count_practitioner), NA,
                                          ifelse(count_practitioner < 0, Inverse.factor, Name)),
         Description.Final_practitioner = ifelse(is.na(count_practitioner), NA,
                                             ifelse(count_practitioner < 0, Inverse.factor.description, Description)))

### invert greater/lesser for research findings (including disaggregated by outcomes)
factors <- factors %>%
  mutate(greater_findings_temp = greater_findings,
         lesser_findings_temp = lesser_findings,
         ma_focus_greater_count_factor_temp = ma_focus_greater_count_factor,
         ma_focus_lesser_count_factor_temp = ma_focus_lesser_count_factor,
         cro_focus_greater_count_factor_temp = cro_focus_greater_count_factor,
         cro_focus_lesser_count_factor_temp = cro_focus_lesser_count_factor,
         greater_findings = ifelse(temp_vote_count_factor < 0, lesser_findings_temp, greater_findings),
         lesser_findings = ifelse(temp_vote_count_factor < 0, greater_findings_temp, lesser_findings),
         ma_focus_greater_count_factor = ifelse(temp_vote_count_factor < 0, ma_focus_lesser_count_factor_temp, ma_focus_greater_count_factor),
         ma_focus_lesser_count_factor = ifelse(temp_vote_count_factor < 0, ma_focus_greater_count_factor_temp, ma_focus_lesser_count_factor),
         cro_focus_greater_count_factor = ifelse(temp_vote_count_factor < 0, cro_focus_lesser_count_factor_temp, cro_focus_greater_count_factor),
         cro_focus_lesser_count_factor = ifelse(temp_vote_count_factor < 0, cro_focus_greater_count_factor_temp, cro_focus_lesser_count_factor)) %>%
  dplyr::select(-c(greater_findings_temp, lesser_findings_temp, 
                   ma_focus_greater_count_factor_temp, ma_focus_lesser_count_factor_temp, 
                   cro_focus_greater_count_factor_temp, cro_focus_lesser_count_factor_temp))

### invert greater/lesser for practitioner findings
factors <- factors %>%
  mutate(greater_practitioner_temp = greater_practitioner,
         lesser_practitioner_temp = lesser_practitioner,
         greater_practitioner = ifelse(count_practitioner < 0, lesser_practitioner_temp, greater_practitioner),
         lesser_practitioner = ifelse(count_practitioner < 0, greater_practitioner_temp, lesser_practitioner)) %>%
  dplyr::select(-c(greater_practitioner_temp, lesser_practitioner_temp))

### mark count_practitioner as NA if == 0
factors <- factors %>%
  mutate(count_practitioner = ifelse(count_practitioner == 0, NA, count_practitioner))

```

## Outcomes

For the summary information on the website, we calculated the number of findings about mass atrocities versus closely-related outcomes.

```{r message = FALSE, warning = FALSE}

##################################

### identify whether gen_effects and factors_article_level articles address MA or CROs

##################################

### select gen_effects article identifiers for each tool
gen_effects_outcomes <- full_article_level %>%
  dplyr::select(Index.Number, Year, Tool, 
                ma_focus_decrease_average, ma_focus_increase_average, ma_focus_no_mixed_average,
                cro_focus_decrease_average, cro_focus_increase_average, cro_focus_no_mixed_average,
                count_adverse, count_without_adverse, effects_type, full_citation) %>% 
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
  dplyr::select(Index.Number, Year, Tool, Outcome, Factor_effect, effects_type, full_citation) %>%
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

### calculate studies by tool
studies_by_tool <- outcomes %>%
  ungroup() %>%
  dplyr::select(Tool, full_citation) %>%
  distinct() %>%
  group_by(Tool) %>%
  mutate(studies_by_tool = n()) %>%
  ungroup() %>%
  dplyr::select(-full_citation) %>%
  distinct()

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

### attach studies by tool
outcomes <- outcomes %>%
  left_join(., studies_by_tool,
            by = "Tool")

### save CSV with outcomes information
write.csv(outcomes, "outcomes 2 June 2025.csv", fileEncoding = "UTF-8")

```

## "Adverse consequences" addendum

For select tools---in particular, comprehensive economic sanctions---"adverse consequences" comprise a large number of findings about the tool's effect on closely-related outcomes. We assume that these findings are relevant to the consideration of when and how to use these tools most effectively. Because these consequences may be distinct from direct outcomes, however, we also draw attention to separate adverse consequences findings from the literature.

``` {r message = FALSE, warning = FALSE}

######################################

### Addendum on average effects with adverse consequences

######################################

### select adverse consequences variables
adverse_consequences <- full_article_level %>%
  ungroup() %>%
  subset(effects_type == "Average") %>%
  dplyr::select(Tool, overall, count, count_adverse, count_without_adverse) %>% 
  distinct()

### save CSV with adverse consequences findings
write.csv(adverse_consequences, "adverse_consequences 2 June 2025.csv", fileEncoding = "UTF-8")

```

## Qualitative description

For the website, we converted the quantitative vote count about each factor to a qualitative range of "Stronger," "Moderate," and "Weaker" evidence. We differentiated between qualitative ratings for the practitioner evidence and the research evidence.

```{r message = FALSE, warning = FALSE}
  
######################################

### Qualitative description of findings

######################################

### research evidence
factors <- factors %>%
  mutate(research_qualitative = ifelse(vote_count_factor > 3, "Stronger",
                                       ifelse(vote_count_factor <= 3 & vote_count_factor > 1, "Moderate",
                                              ifelse(vote_count_factor <= 1 & vote_count_factor >= 0, "Weaker",
                                                     ifelse(vote_count_factor < 0 & vote_count_factor >= -1, "Weaker",
                                                            ifelse(vote_count_factor < -1 & vote_count_factor >= -3, "Moderate",
                                                                   ifelse(vote_count_factor <= -3, "Stronger",
                                                                          NA)))))))

### add "none available" to practitioner factors with 0 research findings
factors <- factors %>%
  mutate(research_qualitative = ifelse(!is.na(count_practitioner) & is.na(count),
                                       "None available", research_qualitative))

### practitioner evidence
factors <- factors %>%
  mutate(practitioner_qualitative = ifelse(abs(proportion_practitioner) > 2/3, "Stronger",
                                           ifelse(abs(proportion_practitioner) <= 2/3 & abs(proportion_practitioner) > 1/3, "Moderate",
                                                  ifelse(abs(proportion_practitioner) <= 1/3, "Weaker",
                                                         NA))))

### create web qualitative language for research findings
factors <- factors %>%
  mutate(research_qualitative_for_web = ifelse(research_qualitative == "Stronger", "Relatively strong",
                                               ifelse(research_qualitative == "Moderate", "Moderate",
                                                      ifelse(research_qualitative == "Weaker", "Relatively weak", NA))))

### create web qualitative language for practitioner findings
factors <- factors %>%
  mutate(practitioner_qualitative_for_web = ifelse(practitioner_qualitative == "Stronger", "Relatively strong",
                                               ifelse(practitioner_qualitative == "Moderate", "Moderate",
                                                      ifelse(practitioner_qualitative == "Weaker", "Relatively weak", NA))))

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
                   "Strategies.the.tool.can.support" = "Strategies.the.tool.can.support",
                   "DIMEL" = "DIMEL")) %>%
  dplyr::rename("Dedoose.name_factor" = "Dedoose.name.x",
                "Dedoose.name_tool" = "Dedoose.name.y")

######################################

### tool and factor categories

######################################

### strategies (for tools)
factors <- factors %>%
  mutate(dissuade = ifelse(grepl("dissuading", Strategies.the.tool.can.support), 1, 
                           ifelse(is.na(Strategies.the.tool.can.support), NA, 0)),
         degrade = ifelse(grepl("degrading", Strategies.the.tool.can.support), 1, 
                          ifelse(is.na(Strategies.the.tool.can.support), NA, 0)), 
         protect = ifelse(grepl("protecting", Strategies.the.tool.can.support), 1, 
                          ifelse(is.na(Strategies.the.tool.can.support), NA, 0)),
         transition = ifelse(grepl("transition", Strategies.the.tool.can.support), 1,
                             ifelse(is.na(Strategies.the.tool.can.support), NA, 0)))

### DIMEL (for tools)
factors <- factors %>%
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
factors <- factors %>%
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

```
         
# Downloading data

The final step is downloading the datasets with our analytic results. First, I export the core datasets; second, I export the datasets disaggregated by prevention tool. 

## Core datasets

```{r message = FALSE, warning = FALSE}

#################################

### write CSVs

#################################

### relocate factor name to the beginning of the dataset, remove CD variable
factors <- factors %>%
  relocate(Name.Final_research, Name.Final_practitioner, tool_name) %>%
  ungroup() %>%
  dplyr::select(-CD)

### factor_level spreadsheet
write.csv(factors, "all_factors 2 June 2025.csv")

### article_level
write.csv(full_article_level, "all_sources 2 June 2025.csv", fileEncoding = "UTF-8")

```

## Disaggregated by prevention tool

```{r message = FALSE, warning = FALSE}

#############################################

### download files without version control labels and tool-specific CSVs

#############################################

### set working directory for CSV files
setwd(work_dir_2023_csv)

### for loop for storage of CSV files - factors

### empty plot list
factors_dataset_list = list()

### tools (remove NA)
tools <- unique(factors$Tool)

### for loop
for(i in 1:length(tools)) {
  
  tools_i <- tools[i]
  
  ### subset and store CSV
  factors_dataset_list[[i]] = factors %>%
    subset(Tool == tools_i)
  print(factors_dataset_list[[i]])
  write.csv(factors_dataset_list[[i]], file = paste0(tools[i], "_factors_final", ".csv"))
}

### for loop for storage of CSV files - article level

### empty plot list
all_sources_dataset_list = list()

### dates
tools <- unique(article_level$Tool)

### for loop
for(i in 1:length(tools)) {
  
  tools_i <- tools[i]
  
  ### subset and store CSV
  all_sources_dataset_list[[i]] = factors %>%
    subset(Tool == tools_i)
  print(all_sources_dataset_list[[i]])
  write.csv(all_sources_dataset_list[[i]], file = paste0(tools[i], "_all_sources_final", ".csv"))
}

#########################

### master spreadsheets

#########################

### factor_level spreadsheet
factors <- factors %>%
  dplyr::select(-Tool)

write.csv(factors, "all_factors_2023.csv")

### full_article_level
write.csv(full_article_level, "all_sources_2023.csv", fileEncoding = "UTF-8")

### CSV with outcomes information
write.csv(outcomes, "outcomes_2023.csv", fileEncoding = "UTF-8")

```

