
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(scales)
library(paletteer)
library(magrittr)
library(glue)


# Get Data ----------------------------------------------------------------


cocktails <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv")
boston_cocktails <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv")


# Functions ---------------------------------------------------------------


p_or <- function(..., sep = " ") {
  paste(..., sep = sep, collapse = "|")
}


# Getting a glimpse at data -----------------------------------------------


names(cocktails)
unique(cocktails$ingredient)

str_detect(unique(cocktails$ingredient), p_or("Rum", "Triple"))

sort(table(cocktails$ingredient))


# Other Vars --------------------------------------------------------------


beverages_find <- c("Vodka", "vodka", "rum", "Rum", "gin", "Gin", "whiskey", "Whiskey", "Bourbon", "Tequila", "Southern Comfort", "Triple sec")


colors <- c(
  "Whiskey" = rgb(244, 142, 18, maxColorValue = 255),
  "Gin" = rgb(90, 180, 203, maxColorValue = 255),
  "Rum" = rgb(204, 55, 33, maxColorValue = 255),
  "Tequila" = rgb(247, 218, 113, maxColorValue = 255),
  "Triple sec" = rgb(180, 72, 44, maxColorValue = 255),
  "Vodka" = rgb(57, 93, 207, maxColorValue = 255)
)


# Dplyr Stuff + Plot ------------------------------------------------------


cocktails %>%
  group_by(drink) %>%
  mutate(count_ing = n()) %>%
  ungroup() %>%
  filter(str_detect(ingredient, p_or(beverages_find))) %>%
  mutate(ingredient = replace(ingredient, str_detect(ingredient, p_or("Vodka", "vodka")), "Vodka")) %>%
  mutate(ingredient = replace(ingredient, str_detect(ingredient, p_or("Rum", "rum")), "Rum")) %>%
  mutate(ingredient = replace(ingredient, str_detect(ingredient, p_or("Bourbon", "Southern Comfort", "Jim Beam", "whiskey")), "Whiskey")) %>%
  mutate(ingredient = replace(ingredient, str_detect(ingredient, "Tequila"), "Tequila")) %>%
  filter(ingredient %in% c("Vodka", "Rum", "Gin", "Whiskey", "Tequila", "Triple sec")) %>%
  group_by(ingredient) %>%
  mutate(count_main = n()) %>%
  group_by(ingredient) %>%
  mutate(mean_ing = round(mean(count_ing), 2)) %>%
  mutate(median_ing = median(count_ing)) %>%
  {
    ggplot(., aes(x = ingredient, y = count_ing, fill = ingredient)) +
      geom_boxplot(colour = "black") +
      scale_fill_manual(values = colors) +
      geom_text(aes(y = 10, label = glue("\n {count_main}")), size = 3.5) +
      geom_text(aes(y = 0.5, label = glue("\n {mean_ing}")), size = 3.5) +
      # geom_text(aes(label = glue("Mean ingredients of these beverages: {mean(count_ing)}", y = 1)))
      annotate("text", x = 1, y = 11, label = "Count of recipies", size = 4) +
      annotate("text", x = 1, y = 0, label = "Mean ingredients", size = 4) +
      scale_y_continuous(limits = c(0, 13), breaks = seq(0, 10, by = 2)) +
      theme_minimal() +
      labs(
        title = "How many ingredients can a cocktail have?",
        subtitle = "Based on 6 basic alcohols",
        caption = "TidyTuesday 26 May 2020\n I might have missed some variants.",
        label = "Legend",
        x = "Base alcohol",
        y = "Amount of ingrediants in a cocktail",
        fill = ""
      ) +
      theme(legend.position = "None")
  }


# Sources  ----------------------------------------------------------------

# https://stackoverflow.com/questions/45088454/how-do-i-access-the-data-frame-that-has-been-passed-to-ggplot/45088522
