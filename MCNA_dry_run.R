# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(msni19) # tata!
library(surveyweights)

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans

library(lubridate)

# load questionnaire inputs
questions <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_questions.csv", 
                      stringsAsFactors=F, check.names=F)

choices <- read.csv("input/questionnaire/SOM_JMCNA_HH_Tool_FIN_2019_settlements_choices.csv", 
                    stringsAsFactors=F, check.names=F)

#load sampling frame
source("source/sampling.R")
# load the sampling frame into an object called samplingframe
# load the cluster sampling frame into an object called clustersamplingframe
# read data
# ###############################   it takes 25 minutes to compute all variables, dont run this all the time
# response <- readRDS("input/data/00-raw_data.RDS")
# 
# names(response)<-to_alphanumeric_lowercase(names(response))
# 
# response <- response %>%
#   select(-ends_with("note")) %>%
#   select(-starts_with("sv_")) %>%
#   select(-starts_with("_"), "_uuid") %>%
#   select(- c(start, end, deviceid, consensus))
# 
# response <- response %>%
#   left_join(select(clustersamplingframe, "P_CODE", "strata"), by = c("settlement" = "P_CODE"))
# response %>% filter(is.na(strata)) %>% nrow()
# ##to be removed when complete dataset and sampling frame
# response <- response %>%
#   filter(!is.na(strata))
# response %>% filter(is.na(strata)) %>% nrow()
# 
# samplingframe <- samplingframe %>% dplyr::filter(strata %in% response$strata)
# ######
# 
# ##source("unicefledd thinkgs)
# 
# # add cluster ids
# 
# # horizontal operations / recoding
# 
# source("source/composite variables/01-horizontal_general.R")
# source("source/composite variables/02-preexisting.R")
# source("source/composite variables/03-education.R")
# source("source/composite variables/04-nutrition.R")
# source("source/composite variables/05-health.R")
# source("source/composite variables/06-shelter_nfi.R")
# source("source/composite variables/07-fsl.R")
# source("source/composite variables/08-wash.R")
# source("source/composite variables/09-protection.R")
# source("source/composite variables/10-mcsi.R")
# source("source/composite variables/11-item_repo.R")
# source("source/composite variables/12-impact.R")
# source("source/composite variables/13-skip_logic.R")
# source("source/composite variables/14-final.R")
# response %>% saveRDS("input/data/02-data_final_scoring31102019.RDS")
# response %>% write.csv("output/dataset_with_var.csv", row.names = F)
############################## END--  it takes 25 minutes to compute all variables, dont run this all the time
response <- readRDS("input/data/02-data_final_scoring31102019.RDS")

#small typo correction
response$vaccination_children[response$vaccination_children == "All"] <- "all"


response_hc_idp <- response %>%
  dplyr::filter(strata %in% samplingframe$strata) %>%
  dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes")

response_refugee_returnee <- response %>% dplyr::select(-c(`_uuid`)) %>%
  dplyr::filter(yes_no_host == "no" & yes_no_idp == "no")

questionnaire <- load_questionnaire(response_hc_idp,questions,choices)

## FS production
source("source/look_up_table.R")
# source("source/maps_FS.R")
# source("source/merge_FS.R")
# source("make_graphs.R")
# source("source/hno_table.R")
# source("aap.R")

analysisplan <- read.csv("input/dap_presentation_redss.csv", stringsAsFactors = F)

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "Population",
                                      sampling.frame.stratum.column = "strata",
                                      data.stratum.column = "strata")

response_hc_idp$general_weights <- strata_weight_fun(response_hc_idp)


results_hc_idp <- from_analysisplan_map_to_output(response_hc_idp, 
                                           analysisplan = analysisplan,
                                           weighting = strata_weight_fun,
                                           cluster_variable_name = "settlement",
                                           questionnaire,
                                           confidence_level = 0.9)

# results_refugee_returnee <- from_analysisplan_map_to_output(response_refugee_returnee,
#                                                             analysisplan = analysisplan_refugee_returnee,
#                                                             questionnaire = questionnaire)


hypegrammaR:::map_to_generic_hierarchical_html(results_hc_idp,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "hc_idp_test.html")


browseURL("hc_idp_test.html")

big_table <- results_hc_idp$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table, "output/big_table.csv", row.names = F)
source("source/make_large_table.R")
source("source/write_results_parameters.R")
# 
# some_results_refugee_returnee <- results_refugee_returnee[1:200]
# 
# 
# # not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# # essentially it handles all the looping over different column values as hierarchies.
# # then each result is visualised by a function passed here that decides how to render each individual result
# # see ?hypegrammaR:::map_to_generic_hierarchical_html
# 
# hypegrammaR:::map_to_generic_hierarchical_html(some_results_refugee_returnee,
#                                                render_result_with = hypegrammaR:::from_result_map_to_md_table,
#                                                by_analysisplan_columns = c("dependent.var","repeat.var.value"),
#                                                by_prefix =  c("",""),
#                                                level = 2,
#                                                questionnaire = questionnaire,
#                                                label_varnames = TRUE,
#                                                dir = "./output",
#                                                filename = "refugee_returnee_test.html")
# 
# browseURL("refugee_returnee_test.html")
