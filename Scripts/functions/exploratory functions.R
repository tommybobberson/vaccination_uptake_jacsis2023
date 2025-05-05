# exploratory_functions.R
# all functions will be prep-ended with the "function_" prefix for clarity


# function wrapper --------------------------------------------------------
# this function serves to hold all the variables to pass to each subsequent function

# data stratification -----------------------------------------------------
# to stratify coverage (by age or dosage) for a certain vaccine (cov or inf)
# by a variable of interest - in essence we are investigating how varying a 
# certain varaible affects the COI's vaccine coverage


function_data_stratify <-
  function(dataset, variable, variable_type, coverage) {
  
  dataset |>
    
    # ensure variables are only for those with valid coverage and variable data
    filter(!is.na({{coverage}}) & !is.na({{variable}})) |>
    
    # coerece variable to proper type for plotting (numeric, factor etc.)
    mutate(across({{variable}}, variable_type)) |>
      
    # count the number of individuals of each coverage for each level of the 
    # variable of interest
    summarise(count = n(), .by = c({{coverage}}, {{variable}})) |>
    
    # calculate the proportion of a coverage level that each level of the
    # variable of interest takes up
    mutate(proportion = count/sum(count), .by = {{variable}}) ->
    stratified_data
  
  return(stratified_data)
}

# function to calculate the 95% confidence intervals
function_calculate_confidence_intervals <-
  function(dataset, variable, method, ...) {
    dataset |>
      mutate(
        # calculate maximum confidence interval
        CI_max = binom.confint(
          x = count,
          n = sum(count),
          methods = method,
          ...
        )$upper,
        
        # calculate minimum confidence interval
        CI_min = binom.confint(
          x = count,
          n = sum(count),
          methods = method,
          ...
        )$lower,
        
        .by = c({{variable}})
      ) -> confidence_interval_data
    
    return(confidence_interval_data)
  }
  

test_function_data_multi_stratify <-
  function(dataset, variables, coverage, dependent_var) {
    
    dataset 
      
      # ensure variables are only for those with valid coverage and variable data
  }

# data plotting -----------------------------------------------------------
# to visualise how the differences in proportions of uptake vary across a variable

# stratified proportional bar graph
function_plot_proportion_cols <-
  function(stratified_data, variable, coverage) {
  
  # pass results of stratified data function to ggplot
  stratified_data |>
      
    ggplot(aes(x = {{coverage}}, y = proportion)) +
    geom_col(aes(fill = {{variable}}), position = "dodge") -> plot
    
    return(plot)
  }

# stratified proportional dot plot
function_plot_proportion_dots  <-
  function(stratified_data, variable, coverage) {
    
    # pass results of stratified data function to ggplot
    stratified_data |>
      
      ggplot(aes(x = {{variable}}, y = proportion)) +
      geom_point(aes(colour = {{coverage}})) -> plot
    
    return(plot)
  }

# adding labels of n for each data point
function_plot_label_count <-
  function(variable, coverage) {
    geom_text(
      aes(group = {{variable}}, label = count), 
      position = position_dodge(width = 0.9), size = 3, vjust = -0.5)
  }

# plotting confidence intervals
function_plot_confidence_intervals <-
  function(variable) {
    geom_errorbar(
      aes(
        ymin = CI_min,
        ymax = CI_max,
        group = {{variable}}
      ),
      position = "dodge"
    )
  }

# plotting paired plots to explore correlation between different variables based on their type
function_plot_corr_pairs <- function(variable1, variable2, df) {
  df <- df |>
    mutate(
      var1 = .data[[variable1]],
      var2 = .data[[variable2]]
    ) |>
    filter(!is.na(var1) & !is.na(var2))
  
  # Both categorical
  if (is.factor(df$var1) & is.factor(df$var2)) {
    return(
      ggplot(df, aes(x = var1, y = var2, group = var1)) +
        
        # calculate the proportion that each level of var 1 takes up for each level of var 2
        geom_count(aes(size = after_stat(n / ave(n ,x))), color = "steelblue") +
        theme_minimal() +
        labs(
          title = paste("A counts plot of", variable2, "against", variable1),
          x = variable1,
          y = variable2
        )
    )
  }
  
  # Both numeric
  if (is.numeric(df$var1) & is.numeric(df$var2)) {
    return(
      ggplot(df, aes(x = var1, y = var2)) +
        geom_point() +
        theme_minimal() +
        labs(
          title = paste("A scatter plot of", variable2, "against", variable1),
          x = variable1,
          y = variable2
        )
    )
  }
  
  # One numeric, one categorical
  if (is.numeric(df$var1) & is.factor(df$var2)) {
    return(
      ggplot(df, aes(x = var2, y = var1)) +
        
        # calculate the proportion of the factor (var2) that each tier of the 
        # numerical variable takes up (var1)
        geom_count(aes(size = after_stat(n / ave(n, x))), color = "steelblue") +
        theme_minimal() +
        labs(
          title = paste("A boxplot of", variable1, "by", variable2),
          x = variable2,
          y = variable1
        )
    )
  }
  
  if (is.numeric(df$var2) & is.factor(df$var1)) {
    return(
      ggplot(df, aes(x = var1, y = var2)) +
        
        # calculate the proportion of the factor (var1) that each tier of the 
        # numerical variable takes up (var2)
        geom_count(aes(size = after_stat(n / ave(n ,x))), color = "steelblue") +
        theme_minimal() +
        labs(
          title = paste("A boxplot of", variable2, "by", variable1),
          x = variable1,
          y = variable2
        )
    )
  }
  
  # Fallback
  stop("Unsupported variable types.")
}