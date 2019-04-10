manage_df <- function(df, row_selection, column_selection){
  df_subset <- df[row_selection, column_selection]
  for (i in 1:ncol(df_subset)) {
    if (is.numeric(df_subset[,i])) {
    print(sum(df_subset[,i]))
    } else {
      print(table(df_subset[,i]))}
  }
  
}

library("datasets")
df <- iris
manage_df(df, c(1,2,3), c(3,4,5))

df2 <- Theoph
manage_df(df2, 1:20, c(T, F))

