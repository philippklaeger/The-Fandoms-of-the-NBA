#' figures.R
#'
#' What this file does:
#' Create your main figures in this script
#'

# --- Load Libraries --- #
library(ggplot2)
library(dplyr)
library(gapminder)

# --- Load Data --- #
# you will probably want to read in your data, 
# this example uses data from the gapminder library

gap_df <- gapminder

# --- Figure 1 --- #
gap_df %>%
    ggplot(aes(x = gdpPercap, 
               y = lifeExp)
           ) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method="lm", size=1.5) +
    theme_bw() +
    xlab("x-axis label") +
    ylab("y-axis label")

ggsave("output/fig-01.pdf")

# --- Figure 2 --- #

gap_df %>%
    filter(continent %in% c("Oceania", "Asia")) %>%
    ggplot(aes(x = year, 
               y = lifeExp,
               by = country)
    ) +
    geom_line() + 
    facet_wrap( ~ continent) +
    theme_bw() +
    xlab("x-axis label") +
    ylab("y-axis label")

ggsave("output/fig-02.pdf")
