#make the graphs

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


response <- readRDS("input/data/02-data_final_scoring09102019.RDS")
#small typo correction
response$vaccination_children[response$vaccination_children == "All"] <- "all"
source("source/sampling.R")
response$region %>%  unique()
response_hc_idp <- response %>%
  dplyr::filter(strata %in% samplingframe$strata) %>%
  dplyr::filter(yes_no_host == "yes" | yes_no_idp == "yes") 
# %>%
#   filter(region == "hiraan")

region_name <- ""

weighting_function <- surveyweights::weighting_fun_from_samplingframe(sampling.frame = samplingframe,
                                                                      data = response_hc_idp,
                                                                      sampling.frame.population.column ="Population",
                                                                      sampling.frame.stratum.column = "strata",
                                                                      data.stratum.column = "strata")


df_to_plot <- response_hc_idp

# sunburst page 1
msni19::sunburst_msni(df_to_plot, 
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = paste0(region_name, "page1_full_sunburst"),
                      path = "output/graphs/")

# line chart page 2
msni19::index_chart(df_to_plot,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = F,
                    print_plot = T,
                    plot_name = paste0(region_name, "page2_msni_line"),
                    path = "output/graphs/")

# bar chart page 2
msni19::index_chart(df_to_plot,
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    index = "msni", 
                    index_max = 4,
                    weighting_function = weighting_function,
                    bar_graph = T,
                    print_plot = T,
                    plot_name = paste0(region_name, "page2_msni_bar"),
                    path = "output/graphs/")



# sunburst page 2 group A : 
msni19::sunburst_msni(dplyr::filter(df_to_plot, population_group == "IDP"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = paste0(region_name, "page2_full_sunburst_A_idp"),
                      path = "output/graphs/")
# sunburst page 2 group B :
msni19::sunburst_msni(dplyr::filter(df_to_plot, population_group == "not_displaced"),
                      msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
                      protection_lsg = "prot_score", shelter_lsg = "snfi_score",
                      wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
                      fsl_wash_branch = T,
                      weighting_function = weighting_function,
                      print_plot = T,
                      plot_name = paste0(region_name, "page2_full_sunburst_B_hc"),
                      path = "output/graphs/")
# # sunburst page 2 group C :
# msni19::sunburst_msni(df_to_plot,
#                       msni = "msni", fsl_lsg = "fsl_score", health_lsg = "health_score", 
#                       protection_lsg = "prot_score", shelter_lsg = "snfi_score",
#                       wash_lsg = "wash_score", capacity_gaps = "mcsi_score", impact = "impact_score", 
#                       fsl_wash_branch = T,
#                       weighting_function = weighting_function,
#                       print_plot = T,
#                       plot_name = "page2_full_sunburst_C",
#                       path = "output/graphs/")

# any lsg graph bar and line
make_bar_line_graph <- function(df, page, lsg_to_graph) {
  graph_name_bar <- paste0(page, "_", lsg_to_graph, "_bar")
  bar_gr <- msni19::index_chart(df,
                                group = "yes_no_host",
                                group_order = c("yes", "no"),
                                group_labels = c("Non-displaced","IDP"),
                                index = lsg_to_graph, 
                                index_max = 4,
                                weighting_function = weighting_function,
                                bar_graph = T,
                                print_plot = T,
                                plot_name = paste0(region_name, graph_name_bar),
                                path = "output/graphs/")
  # line_name_bar <- paste0(page, "_", lsg_to_graph, "_line")
  
  # line_gr <- msni19::index_chart(df,
                                 # group = "yes_no_host",
                                 # group_order = c("yes", "no"),
                                 # group_labels = c("Non-displaced","IDP"),
                                 # index = lsg_to_graph, 
                                 # index_max = 4,
                                 # weighting_function = weighting_function,
                                 # bar_graph = F,
                                 # print_plot = T,
                                 # plot_name = paste0(region_name, line_name_bar),
                                 # path = "output/graphs/")
  print(bar_gr)
  # print(line_gr)
}


simple_chart_and_page <- data.frame(page = paste0("page", 3:10),
                                       lsg = c("wash_score", "health_score", "snfi_score", "edu_score", "prot_score",
                                            "mcsi_score", "pev_score", "impact_score"),
                                            stringsAsFactors = F)

mapply(make_bar_line_graph, page = simple_chart_and_page$page, lsg_to_graph = simple_chart_and_page$lsg, 
       MoreArgs = list(df = df_to_plot))

#page 11 radar
msni19::radar_graph(df_to_plot, 
                    lsg = c("edu_score", 
                            "nut_score", 
                            "health_score", 
                            "snfi_score",
                            "fsl_score",
                            "wash_score", 
                            "prot_score"),
                    lsg_labels = c("Edu",
                                   "Nutri",
                                   "Health",
                                   "SNFI",
                                   "Food sec", 
                                   "WASH", 
                                   "Prot"),
                    group = "yes_no_host",
                    group_order = c("yes", "no"),
                    group_labels = c("Non-displaced","IDP"),
                    weighting_function = weighting_function,
                    print_plot = T,
                    plot_name = paste0(region_name, "page11_radar"),
                    legend_position = "bottom", 
                    path = "output/graphs/")

#page 11 line graph
lsg <- c("edu_score", "nut_score",
         "health_score", "snfi_score", "fsl_score", 
         "wash_score", "prot_score")

# Making graph of % of households by # of indices that are >= 3
msni19::severity_lines(df_to_plot,
                       lsg,
                       group = "yes_no_host",
                       group_order = c("yes", "no"),
                       group_labels = c("Non-displaced","IDP"),
                       weighting_function = weighting_function,
                       print_plot = T,
                       plot_name = paste0(region_name, "page11_over3"),
                       path = "output/graphs/")

#page 11 venn
# venn diagram of households with any LSG >= 3 (REACH red) and those with capacity gaps >= 3 (REACH light grey)

msni19::venn_msni(df_to_plot, 
                  lsg = c("edu_score", 
                          "snfi_score", 
                          "fsl_score", 
                          "health_score", 
                          "prot_score",
                          "wash_score",
                          "nut_score"),
                  capacity_gaps = "mcsi_score",
                  weighting_function = weighting_function,
                  print_plot = T,
                  plot_name = paste0(region_name, "page11_venn"),
                  path = "output/graphs/")


#page 11 intersection
msni19::index_intersections(df_to_plot,
                            lsg =  c("edu_score", "snfi_score", "fsl_score", "health_score",
                                     "prot_score", "wash_score", "nut_score"),
                            lsg_labels = c("Education",
                                           "Shelter",
                                           "Food",
                                           "Health",
                                           "Protection",
                                           "WASH",
                                           "Nutrition"),
                            weighting_function = weighting_function,
                            print_plot = F,
                            plot_name = paste0(region_name, "page11_intersection"),
                            path = "output/graphs/")


