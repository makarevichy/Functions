likelihood <- function(df, group_col, target_col, smoothing = FALSE, alpha = 0, simplify = T, n_round = NULL){
  if(!is.data.frame(df)) stop("'df' must be a data.frame class")
  if(smoothing == TRUE && alpha == 0) stop("'alpha' cannot be 0. Please, change alpha.")
  name_col <- paste0(deparse(substitute(group_col)), '_new_1')
  
  require(dplyr)
  group_col <- enquo(group_col)
  target_col <- enquo(target_col)
  mean_t <- df %>% summarise(mean(!!target_col, na.rm = TRUE)) %>% .[1,1]
  df_group <- df %>% 
    group_by(!!group_col) %>% 
    count() %>% 
    left_join(df %>% 
                group_by(!!group_col) %>% 
                summarise(mean_group = mean(!!target_col, na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(target_mean = mean_group / n)
  
  if(smoothing){
    df_group <- df_group %>%
      mutate(smoothing_likelihood = ((target_mean * n) + (mean_t * alpha)) / (n + alpha))
    vec <- df %>% 
      select(!!group_col) %>% 
      left_join(df_group) %>% 
      select(.data$smoothing_likelihood) %>% 
      pull() %>% 
      round(5)
    if(simplify){
      return(vec)
    } else {
      vec <- df %>% 
        bind_cols(new = vec)
      names(vec)[length(vec)] <- name_col
      return(vec)
    }
  }
  vec <- df %>%
    select(!!group_col) %>% 
    left_join(df_group) %>% 
    select(.data$target_mean) %>% 
    pull() %>% 
    round(5)
  if(simplify){
    return(vec)
  } else {
    vec <- df %>% 
      bind_cols(new = vec)
    names(vec)[length(vec)] <- name_col
    return(vec)
  }
}


likelihood(df = mtcars, group_col = cyl, target_col = am, smoothing = T, alpha = 5, simplify = F)
likelihood(df = mtcars, group_col = cyl, target_col = vs, simplify = T)
