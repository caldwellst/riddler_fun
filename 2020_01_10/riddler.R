# Riddler Classic
rate <- setNames(as.list(1:26), letters)

pricer <- function(x, rate) {
  rate[[x]]
}

converter <- function(x, rate) {
  x <- english::english(x)
  x <- stringr::str_remove_all(x, "( )|(\\-)")
  x <- unlist(stringr::str_split(x, ""))
  sum(purrr::map_dbl(x, pricer, rate))
}

num_vals <- 1:1000
alph_vals <- purrr::map_dbl(num_vals, converter, rate)

df <- tibble::tibble(
  num_vals = num_vals,
  alph_vals = alph_vals
) %>%
  mutate(num_less = num_vals < alph_vals)

# Plotting

library(ggplot2)

ggplot(df, aes(x = num_vals, y = alph_vals, color = num_less)) +
  geom_point() + 
  scale_color_manual(values = c("grey", "red")) +
  geom_abline(slope = 1, intercept = 0, size = 1) +
  theme_minimal() +
  labs(x = "Numeric value",
       y = "Alphanumeric value (calculated)",
       color = "Number is less than\nalphanumeric value")

df %>% filter(num_less) %>% pull(num_vals) %>% max
#> 279