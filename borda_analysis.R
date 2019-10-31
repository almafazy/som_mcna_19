source("MCNA_dry_run.R")
source("functions/borda_count.R")

borda_script <- readr::read_csv("input/borda/borda_analysis.csv")

borda_script <- borda_script[1:2,]
borda_script$repeat_var <- "statex7"
borda_script 
borda_result <- borda_applier(script = borda_script, df = response_hc_idp,weighting_function =   strata_weight_fun) 

borda_result %>% filter(repeat_var == "somaliland") %>% write.csv("redss_borda_count.csv", row.names = F)


borda_count(df = response_hc_idp %>% filter(statex7 == "somaliland"), 
            columns = c("school_barrier_first", "school_barrier_second", "school_barrier_third"),
            weighting_function = strata_weight_fun, 
            exclude = c(""), 
            ranks = 3, 
            return_names = T)
              
            
borda_count(df = response_hc_idp %>% filter(statex7 == "somaliland"), 
            columns = c("health_barrier_first", "health_barrier_second", "health_barrier_third"),
            weighting_function = strata_weight_fun, 
            exclude = c(""), 
            ranks = 3, 
            return_names = T)          



make_na <- function(x, vals) {
  x[x %in% vals] <- NA
  x
}

#' function to get weighted borda count for any number of vectors
#'
#' @param df data frame
#' @param columns vectors to be ranked
#' @param weights weight vector. Should be the same length as vectors provided for ranking
#' @param exclude vector of values to exclude from ranking analysis
#' @param ranks number of ranks to be calculated
borda_count <- function(df, columns, weighting_function = NULL, exclude = NULL, ranks = 3, return_names = T) {
  df <- mutate_at(df, columns, make_na, "") %>%
    filter_at(columns, any_vars(!is.na(.)))
  
  if (is.null(weighting_function)) {
    weights <- rep(1, nrow(df))
  } else {
    weights <- weighting_function(df)
  }
  
  df <- select(df, columns)
  
  vals <- map(df, ~ unique(.x[!is.na(.x)]))
  vals <- unique(unlist(vals)) # getting unique values to rank
  ranks <- min(ranks, length(vals)) # ensuring that end result doesn't have NA values for calculating for non-existent rankings
  table <- map_dfc(vals, ~ tibble(!!(.x) := map_dbl(df, function(x) sum((x == .x) * weights, na.rm = T)))) %>% # counting the weighted occurences of each value
    mutate_all(~ .x * (length(vals) - 1:n() + 1))  %>% # multiplying row values by their borda rank
    summarize_all(sum)
  
  table <- table[,order(-table[1,])] # reordering based on score
  
  table <- table %>%
    mutate_all(~ scales::percent(.x / rowSums(table)))# getting the total vote score for each item
  
  names <- paste(names(table)[1:ranks], collapse = " ") # getting the total borda count for each
  percents <- paste(table[1, 1:ranks], collapse = " ")
  if (return_names) {
    table
  } else {
    percents
  }
}