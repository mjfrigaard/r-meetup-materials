
library(tidyverse)

CatsVsDogs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-11/cats_vs_dogs.csv")

# shrink
CatsVsDogs %>% 
  select(state, starts_with("n_")) %>% 
  arrange(desc(n_households)) %>% 
  head(5) -> CvD

# widen
CvD %>% 
  pivot_longer(-state, 
               names_to = "metric", 
               values_to = "value") %>% 
  pivot_wider(names_from = "state", values_from = "value") %>% 
  mutate(metric = str_replace_all(string = metric, 
                                  pattern = "n_", 
                                  replacement = "no_of_")) -> CatVsDogWide