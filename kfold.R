library(caret)
data("GermanCredit")

k <- 2
x_list <- createFolds(mtcars[, 'mpg'], k = k)
df <- map(x_list, function(ki) mtcars[ki, ])
keys <- map(df, createFolds, k, list = T)
vec <- vector('list', k)
for(i in seq_along(keys)){
  vec[[i]] <- lapply(keys[[i]], function(x) df[[i]][x, ])
  vec[[i]] <- lapply(vec[[i]], function(x) likelihood(x, cyl, mpg, T, 5))
  vec[[i]] <- map_df(vec[[i]], bind_rows)
}
map_df(vec, bind_rows)


k <- 3
x_list <- createFolds(GermanCredit[, 'Class'], k = k)
df <- map(x_list, function(ki) GermanCredit[ki, ])
keys <- map(df, function(x) createFolds(x[, 'Class'], k, list = T))
vec <- vector('list', k)
for(i in seq_along(keys)){
  vec[[i]] <- lapply(keys[[i]], function(x) df[[i]][x, ])
  vec[[i]] <- lapply(vec[[i]], function(x) likelihood(x, InstallmentRatePercentage, Class, T, 30))
  vec[[i]] <- map_df(vec[[i]], bind_rows)
}
x <- map_df(vec, bind_rows)

x %>% 
  mutate(new = likelihood(x, InstallmentRatePercentage, new, T, 30)$new)
mutate(new = likelihood(x, InstallmentRatePercentage, new, T, 30)$new)
likelihood(x, InstallmentRatePercentage, new, T, 30) %>% 
  select(InstallmentRatePercentage, new, new1)
