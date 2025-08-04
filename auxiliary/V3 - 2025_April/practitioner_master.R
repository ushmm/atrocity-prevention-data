### load packages
library(groundhog)

groundhog.library("stringr", "2022-07-13") ### edit strings
groundhog.library("tidyr", "2022-07-13") ### data wrangling
groundhog.library("fuzzyjoin", "2022-07-13") ### fuzzy text joins
groundhog.library("reshape", "2022-07-13") ### reshape datasets
groundhog.library("dplyr", "2022-07-13") ### data wrangling

### load working directory
work_dir <- "G:/.shortcut-targets-by-id/1gvhqlwU_FnML3ntwXRBg_r48BjM6CRsK/Lessons learned/Practitioner knowledge/Survey/Targeted Sanctions/Analysis"
setwd(work_dir)
rm(work_dir)

### load CSV with practitioner knowledge survey results
practitioner <- read.csv("practitioner_survey.csv", na.strings = "")

### convert column names to values from first row
colnames(practitioner) <- practitioner[1, ]

### subset to (1) remove observations that have not been completed, and
### (2) add index value
practitioner <- practitioner %>%
  subset(Finished == "TRUE") %>%
  mutate(practitioner_index = seq(1:nrow(.)))

### create data frame of practitioner names
names <- practitioner %>%
  dplyr::select(c(practitioner_index, `Respondent first name`, `Respondent last name`))

### (3) remove columns up to C17, and 
### (4) remove practitioner names
practitioner <- practitioner[, -c(1:17)] %>%
  dplyr::select(-c(`Respondent first name`, `Respondent last name`)) %>%
  dplyr::relocate(practitioner_index)

#######################################################

### Factors

#######################################################

### pivot to longer dataframe including interviewee
practitioner_long <- practitioner %>%
  pivot_longer(cols = -c(practitioner_index, 2:3),
               names_to = "names",
               values_to = "values")

### identify the rows that include (1) direction, (2) confidence ratings, (3) transcript
direction_confidence <- practitioner_long %>%
  mutate(keep = ifelse(grepl("more or less", names) |
                         grepl("confidence", names) |
                         grepl("transcript", names) &
                         !grepl("Effectiveness across cases", names), 
                       1, 0)) %>%
  subset(keep == 1) %>%
  dplyr::select(-keep)

### replace "more or less likely..." with "direction," 
### split "names" column to identify the factor number (e.g. CF1, DF2),
### remove factors with NA values
direction_confidence <- direction_confidence %>%
  mutate(names = gsub("more or less likely to prevent atrocities\\?", "direction", names)) %>%
  separate(names, sep = " ", c("factor_label", "names")) %>%
  subset(!is.na(values))

### pivot wider
direction_confidence <- direction_confidence %>%
  pivot_wider(names_from = names,
              values_from = values)

### identify the rows that include factors
factors_wide <- practitioner_long %>%
  mutate(factor = ifelse(grepl("name", names), 1, 0)) %>%
  subset(factor == 1 & !grepl("respondent", names) & !grepl("contacts", names)) %>%
  subset(!is.na(values)) %>%
  dplyr::select(-factor) %>%
  arrange(values)

### create variables to indicate factors
factors_wide <- factors_wide %>%
  mutate(third_party_support_coordination = ifelse(grepl("multilat", values, ignore.case = TRUE) |
                                 grepl("coordination", values, ignore.case = TRUE), 
                               1, 0),
         un_sanctions_support = ifelse(grepl("UN leads", values, ignore.case = TRUE) |
                                         grepl("UNSC", values, ignore.case = TRUE),
                                       1, 0),
         commitment = ifelse(!grepl("ideological", values, ignore.case = TRUE) & 
                               grepl("commit", values, ignore.case = TRUE) | 
                               grepl("comit", values, ignore.case = TRUE) |
                               grepl("enforc", values, ignore.case = TRUE),
                             1, 0),
         communication = ifelse(grepl("communic", values, ignore.case = TRUE) | 
                                  grepl("messag", values, ignore.case = TRUE) |
                                  grepl("off-ramp", values, ignore.case = TRUE),
                                1, 0),
         target_intl_exposure = ifelse(!grepl("individual", values, ignore.case = TRUE) &
                                  !grepl("military", values, ignore.case = TRUE) &
                                  grepl("financ", values, ignore.case = TRUE) | 
                                    grepl("family members studying", values, ignore.case = TRUE) |
                                    grepl("travel abroad", values, ignore.case = TRUE)|
                                    grepl("US dollar", values, ignore.case = TRUE) |
                                    grepl("international system", values, ignore.case = TRUE) |
                                  grepl("wealthy", values, ignore.case = TRUE),
                                1, 0),
         multi_tool = ifelse(grepl("multi", values, ignore.case = TRUE) &
                                   grepl("tool", values, ignore.case = TRUE) &
                                   !grepl("multilat", values, ignore.case = TRUE) |
                                   grepl("whole-of-government", values, ignore.case = TRUE) |
                                   grepl("strategy", values, ignore.case = TRUE) |
                               grepl("diplomatic process", values, ignore.case = TRUE) |
                               grepl("military counterparts", values, ignore.case = TRUE),
                                 1, 0),
         knowledge_information = ifelse(grepl("intell", values, ignore.case = TRUE) |
                                        grepl("good information", values, ignore.case = TRUE) |
                                          grepl("evidence", values, ignore.case = TRUE),
                               1, 0),
         reputation_target = ifelse(grepl("reputation", values, ignore.case = TRUE) |
                               grepl("legitimacy", values, ignore.case = TRUE),
                             1, 0),
         effective_process = ifelse(grepl("complicated", values, ignore.case = TRUE) |
                                      grepl("effective", values, ignore.case = TRUE) | 
                                      grepl("impact", values, ignore.case = TRUE),
                                    1, 0),
         broad_authorities = ifelse(grepl("multi", values, ignore.case = TRUE) &
                                      grepl("sanctions", values, ignore.case = TRUE) &
                                      !grepl("multilat", values, ignore.case = TRUE) |
                                      grepl("blocking", values, ignore.case = TRUE) |
                                      grepl("authorities", values, ignore.case = TRUE) |
                                      grepl("network", values, ignore.case = TRUE) |
                                      grepl("full set", values, ignore.case = TRUE) |
                                      grepl("secondary sanctions", values, ignore.case = TRUE) |
                                      grepl("economic base", values, ignore.case = TRUE) |
                                      grepl("individual actor", values, ignore.case = TRUE),
                                    1, 0),
         identifiable_perpetrators = ifelse(grepl("identifiable", values, ignore.case = TRUE) |
                                              grepl("key", values, ignore.case = TRUE),
                                            1, 0),
         experience = ifelse(grepl("agencies", values, ignore.case = TRUE) |
                               grepl("lessons learned", values, ignore.case = TRUE),
                                   1, 0),
         personal_priorities = ifelse(grepl("personal priorities", values, ignore.case = TRUE),
                                      1, 0),
         implementer_leverage = ifelse(grepl("economic centers", values, ignore.case = TRUE),
                                         1, 0),
         implementer_reputation = ifelse(grepl("suasion", values, ignore.case = TRUE),
                                         1, 0),
         history_tool = ifelse(grepl("history", values, ignore.case = TRUE),
                          1, 0),
         target_great_power = ifelse(grepl("great power", values, ignore.case = TRUE),
                                     1, 0),
         target_authoritarian = ifelse(grepl("authoritarian", values, ignore.case = TRUE),
                                1, 0),
         target_enablers = ifelse(grepl("providing services", values, ignore.case = TRUE),
                           1, 0),
         access_resources = ifelse(grepl("illicit", values, ignore.case = TRUE) |
                                     grepl("replacements", values, ignore.case = TRUE),
                                       1, 0),
         state_actor = ifelse(grepl("state actor", values, ignore.case = TRUE),
                              1, 0),
         policy_objective = ifelse(grepl("policy objective", values, ignore.case = TRUE),
                                   1, 0),
         after_atrocities = ifelse(grepl("after mass atrocities", values, ignore.case = TRUE),
                                   1, 0),
         atrocity_justifying_ideology = ifelse(grepl("ideological", values, ignore.case = TRUE),
                           1, 0),
         intl_hr_standards = ifelse(grepl("norms", values, ignore.case = TRUE),
                                    1, 0),
         legal_authority = ifelse(grepl("legal", values, ignore.case = TRUE),
                                  1, 0),
         target_interests_salient = ifelse(grepl("care", values, ignore.case = TRUE) &
                                    grepl("about", values, ignore.case = TRUE), 
                                  1, 0),
         target_awareness = ifelse(grepl("targets are aware", values, ignore.case = TRUE),
                                   1, 0),
         ma_policy_goal = ifelse(grepl("specific policy goal", values, ignore.case = TRUE), 
                                        1, 0), 
         humanitarian_access = ifelse(grepl("humanitarian", values, ignore.case = TRUE), 
                                      1, 0),
         influential_perpetrators = ifelse(grepl("power and discretion", values, ignore.case = TRUE) |
                                             grepl("elite", values, ignore.case = TRUE), 
                                           1, 0),
         sanctions_escalate = ifelse(grepl("progressive", values, ignore.case = TRUE),
                                     1, 0),
         rapid_use = ifelse(grepl("quickly", values, ignore.case = TRUE), 
                            1, 0),
         military_activity = ifelse(grepl("military activity", values, ignore.case = TRUE),
                                    1, 0))

### separate out factor label
factors_wide <- factors_wide %>%
  separate(names, sep = " ", c("factor_label", "names")) %>%
  dplyr::select(-names)

### convert wide factor dataset to long
factors_base <- factors_wide %>%
  dplyr::select(-c(`Date of intervew`)) %>%
  pivot_longer(cols = -c(Tool, practitioner_index, factor_label, values),
               names_to = "factor",
               values_to = "yes_no") %>%
  subset(yes_no == 1) %>%
  dplyr::select(-yes_no)

### merge with confidence measures
factors_base <- factors_base %>%
  left_join(., direction_confidence,
            by = c("practitioner_index", "factor_label",
                   "Tool" = "Tool")) %>%
  dplyr::select(-c(`Date of intervew`))

############################################################

### Analytic guide for practitioner data

############################################################

### load Word doc using OFFICER
library(officer)
doc_analytic_guide <- read_docx("[for editing] Practitioner factor guide.docx")

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

#######################################################

### Merge analytic guide with practitioner data

#######################################################

### left join practitioner data to analytic_guide_wider
factors <- factors_base %>%
  left_join(., analytic_guide_wider,
            by = c("factor" = "Interview name"))

### remove NA value (for implementer_reputation)
factors <- factors %>%
  subset(!is.na(Name))

### remove confidence assessment
factors <- factors %>%
  dplyr::select(-confidence)

### correct "less" direction for leverage
factors <- factors %>%
  mutate(direction = ifelse(Name == "Implementer has strong leverage", "More", direction))

### invert after_atrocities
factors <- factors %>%
  mutate(direction = ifelse(factor == "after_atrocities" & direction == "More", "Less",
                            ifelse(factor == "after_atrocities" & direction == "Less", "More", 
                                   direction)))

### remove "broad authorities" interview with contradictory directions
factors <- factors %>%
  group_by(practitioner_index, factor) %>%
  mutate(contradictory = ifelse(length(unique(direction)) > 1, 1, 0)) %>%
  subset(contradictory == 0) %>%
  dplyr::select(-contradictory) %>%
  ungroup()

########################################### 

### consensus calculation

###########################################

### rename "values" column to "original_text"
factors <- factors %>%
  dplyr::rename(original_text = values)

### create separate dataframe for original_text and practitioner_index
factor_text <- factors %>%
  ungroup() %>%
  dplyr::select(c(Name, practitioner_index, original_text, 
                  transcript, direction))

### remove factor_label, original_text, transcript, and deduplicate
factors <- factors %>%
  dplyr::select(-c(factor_label, original_text, transcript)) %>%
  distinct()

### remove duplicate references to factors
factors <- factors %>%
  ungroup() %>%
  dplyr::select(-c(factor)) %>%
  distinct()

### add variables summarizing greater, lesser, and no_mixed, calculate count (greater - lesser, since there are no "no or mixed" in this batch)
factors <- factors %>%
  group_by(Name) %>%
  mutate(greater_practitioner = ifelse(direction == "More", 1, 0),
         lesser_practitioner = ifelse(direction == "Less", 1, 0),
         no_mixed_practitioner = ifelse(direction == "No or mixed effect", 1, 0)) %>%
  mutate(count_practitioner = sum(greater_practitioner) - sum(lesser_practitioner)) %>%
  mutate(greater_practitioner = sum(greater_practitioner),
         lesser_practitioner = sum(lesser_practitioner),
         no_mixed_practitioner = sum(no_mixed_practitioner)) %>%
  ungroup()

### calculate proportion of respondents who cite factor
factors$number_practitioners <- length(unique(factors$practitioner_index))

factors <- factors %>%
  mutate(proportion_practitioner = count_practitioner / number_practitioners)

### left_join original text
factors <- factors %>%
  left_join(., factor_text,
            by = c("practitioner_index", "Name", "direction"))

########################################################

### Export data

########################################################

### write csv file disaggregated by practitioner
factors_interview_level <- factors

write.csv(factors_interview_level, "factors_interview_level.csv")

### write csv file at factor level
factors_factor_level <- factors %>%
  ungroup() %>%
  dplyr::select(-c(practitioner_index, direction, original_text, transcript)) %>%
  distinct()

write.csv(factors_factor_level, "factors_factor_level.csv")

###################################

### Visualizations - demographics

###################################

### add fonts
library(showtext)
font_add(family = "merlo_label", regular = "D:/Google Drive/CPG/Style Guide/MerloTx-Regular.otf")
showtext_auto()

###################

### years of experience

###################

### experience variables
experience <- practitioner %>%
  dplyr::select(c(practitioner_index,
                  `Presidential administration(s) - Selected Choice`,
                  `Presidential administration(s) - Other - Text`,
                  `Agency affiliation(s) - Selected Choice`,
                  `Agency affiliation(s) - Other - Text`,
                  `Years working on the tool in government`, 
                  `Years working on the tool out of government`))

### correct years of experience
experience <- experience %>%
  mutate(`Years working on the tool out of government` = ifelse(`Years working on the tool out of government` == "4-5", 4.5, `Years working on the tool out of government`),
         `Years working on the tool out of government` = ifelse(is.na(`Years working on the tool out of government`), 0, `Years working on the tool out of government`),
         `Years working on the tool in government` = ifelse(`Years working on the tool in government` == "13-15", 14, `Years working on the tool in government`))

experience$`Years working on the tool in government` <- as.numeric(experience$`Years working on the tool in government`)
experience$`Years working on the tool out of government` <- as.numeric(experience$`Years working on the tool out of government`)

### calculate mean, min, max in government
mean(experience$`Years working on the tool in government`, na.rm = TRUE)
min(experience$`Years working on the tool in government`, na.rm = TRUE)
max(experience$`Years working on the tool in government`, na.rm = TRUE)

### calculate mean, min, max out of government
mean(experience$`Years working on the tool out of government`, na.rm = TRUE)
min(experience$`Years working on the tool out of government`, na.rm = TRUE)
max(experience$`Years working on the tool out of government`, na.rm = TRUE)

### pivot experience to longer
experience_longer <- experience %>%
  dplyr::select(practitioner_index, 
                `Years working on the tool in government`,
                `Years working on the tool out of government`) %>%
  tidyr::pivot_longer(cols = c(`Years working on the tool in government`,
                               `Years working on the tool out of government`),
                      names_to = "experience_type",
                      values_to = "years")

### table of experience - dummy variables
experience_longer <- experience_longer %>%
  mutate(zero_to_five = ifelse(years >= 0 & years < 5, 1, 0),
         five_to_ten = ifelse(years >= 5 & years < 10, 1, 0),
         ten_to_fifteen = ifelse(years >= 10 & years < 15, 1, 0),
         fifteen_to_twenty = ifelse(years >= 15 & years < 20, 1, 0),
         greater_than_twenty = ifelse(years >= 20, 1, 0)) %>%
  subset(!is.na(years))

### table of experience - sum
experience_sum <- experience_longer %>%
  group_by(experience_type) %>%
  mutate(zero_to_five = sum(zero_to_five),
         five_to_ten = sum(five_to_ten),
         ten_to_fifteen = sum(ten_to_fifteen),
         fifteen_to_twenty = sum(fifteen_to_twenty),
         greater_than_twenty = sum(greater_than_twenty)) %>%
  dplyr::select(-c(practitioner_index, years)) %>%
  distinct() %>%
  transpose()

### set first row as column names
colnames(experience_sum) <- experience_sum[1, ]
experience_sum <- experience_sum[-1, ]

### add column with year categories
years <- c("0 - 5",
           "5 - 10",
           "10 - 15",
           "15 - 20",
           "> 20")

### bind years to experience dataframe
experience_sum <- cbind(years, experience_sum)

### print html of table
library(kableExtra)
kable(experience_sum, "html") %>%
  save_kable("experience_table.html")

### create ggplot item
experience_plot <- experience_longer %>%
  mutate(years = ifelse(is.na(years), 0, years)) %>%
  ggplot(aes(x = years, fill = experience_type))

### histogram
experience_plot +
  geom_histogram(position = position_dodge(width = 2), 
                 binwidth = 1,
                 bins = 5) +
  theme_bw() +
  scale_fill_manual(name = "Experience type",
                    breaks = c("Years working on the tool in government",
                               "Years working on the tool out of government"),
                    labels = c("In government", "Out of government"),
                    values = c("#F37021", "#85878A")) +
  theme(text = element_text(family = "merlo_label", size = 20)) +
  labs(x = "Number of years of experience",
       y = "Frequency")

###################

### administration

###################

### administration dummies
experience <- experience %>%
  mutate(gw_bush = ifelse(grepl("GW Bush", `Presidential administration(s) - Selected Choice`), 1, 0),
         clinton = ifelse(grepl("Clinton", `Presidential administration(s) - Selected Choice`), 1, 0),
         obama = ifelse(grepl("Obama", `Presidential administration(s) - Selected Choice`), 1, 0),
         hw_bush = ifelse(grepl("HW", `Presidential administration(s) - Other - Text`), 1, 0),
         trump = ifelse(grepl("Trump", `Presidential administration(s) - Selected Choice`), 1, 0))

### admin data frame
library(forcats)
admin = c("HW Bush", "Clinton", "GW Bush", "Obama", "Trump")
count_admin = c(sum(experience$hw_bush),
                sum(experience$clinton),
                sum(experience$gw_bush),
                sum(experience$obama),
                sum(experience$trump))
admins <- data.frame(admin, count_admin) %>%
  mutate(admin = fct_relevel(admin, "HW Bush",
                             "Clinton",
                             "GW Bush",
                             "Obama",
                             "Trump"))

### print html of table
kable(admins, "html") %>%
  save_kable("admins_table.html")

### create ggplot item
admins_plot <- admins %>%
  ggplot(aes(x = admin, y = count_admin))

### plot
admins_plot +
  theme_bw() +
  geom_bar(stat = "identity", 
           fill = "#F37021") +
  theme(text = element_text(family = "merlo_label", size = 20)) +
  labs(x = "Administration",
       y = "Number of respondents")

###################

### agency

###################

### agency dummies
experience <- experience %>%
  mutate(wh = ifelse(grepl("White House", `Agency affiliation(s) - Selected Choice`), 1, 0),
         treasury = ifelse(grepl("Treasury", `Agency affiliation(s) - Selected Choice`), 1, 0),
         us_un = ifelse(grepl("US Mission", `Agency affiliation(s) - Selected Choice`), 1, 0),
         state = ifelse(grepl("State", `Agency affiliation(s) - Selected Choice`), 1, 0),
         doj = ifelse(grepl("DOJ", `Agency affiliation(s) - Other - Text`), 1, 0),
         dhs = ifelse(grepl("DHS", `Agency affiliation(s) - Other - Text`), 1, 0),
         cia = ifelse(grepl("CIA", `Agency affiliation(s) - Other - Text`), 1, 0))

### agencies data frame
agency = c("White House", "Treasury", "State", "US/UN", "DOJ", "DHS", "CIA")
count_agency = c(sum(experience$wh),
                 sum(experience$treasury),
                 sum(experience$us_un),
                 sum(experience$state),
                 sum(experience$doj),
                 sum(experience$dhs),
                 sum(experience$cia))

agencies <- data.frame(agency, count_agency)

### print html of table
kable(agencies, "html") %>%
  save_kable("agencies_table.html")

### create ggplot item
agencies_plot <- agencies %>%
  ggplot(aes(x = agency, y = count_agency))

### plot
agencies_plot +
  theme_bw() +
  geom_bar(stat = "identity", 
           fill = "#F37021") +
  ylim(0, 15) +
  theme(text = element_text(family = "merlo_label", size = 20)) +
  labs(x = "Agency",
       y = "Number of respondents")

#######################################################

### Distribution of perspectives on overall effects

#######################################################

### pivot to longer dataframe
overall <- practitioner %>%
  pivot_longer(cols = -c(practitioner_index, 2:3),
               names_to = "names",
               values_to = "values")

### subset to overall effectiveness response
overall <- overall %>%
  subset(names == "How often does the tool succeed at preventing mass atrocities?")

### ggplot

# bar chart

# USHMM orange: F37021
# USHMM grey: 85878A

options <- data.frame(options = c("Always", "Often", "Sometimes", "Rarely", "Never", "No response"))
overall_graph <- overall %>%
  group_by(values) %>%
  summarize(n = n()) %>%
  mutate(values = ifelse(is.na(values), "No response", values)) %>%
  left_join(options, ., by = c("options" = "values"))

### remove "no response"
overall_graph <- overall_graph %>%
  subset(options != "No response")

### change NA values to 0
overall_graph$n <- ifelse(is.na(overall_graph$n), 0, overall_graph$n)

### plot
overall_graph %>%
  mutate(options = factor(options,
                          levels = c("Always", "Often", "Sometimes", "Rarely", "Never", "No response"))) %>%
  ggplot() +
  geom_bar(aes(x = options,
               y = n),
           fill = "#F37021",
           stat = "identity",
           width = 0.2,
           position = "stack") +
  scale_x_discrete(breaks = c("Always", "Often", "Sometimes", "Rarely", "Never"),
                   labels = c("Always", "Often", "Sometimes", "Rarely", "Never")) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_flip() +
  theme_bw() +
  theme(aspect.ratio = 1/3,
        text = element_text(family = "merlo_label", size = 20)) +
  labs(x = "Frequency of success",
       y = "Number of responses",
       caption = "Note: One interviewee declined to respond to this question.")

