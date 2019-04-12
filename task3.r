manage_df <- function(df, row_selection, column_selection, operation){
  df_subset <- data.frame(df[row_selection, column_selection])
  calculation_function <- function(x, operation){
    if (is.numeric(x)) {
      return(operation(x))
    } else {
      return(table(x))
      }
    }
  applying <- sapply(df_subset, calculation_function, operation)
  return(list(df_subset, applying))
}

