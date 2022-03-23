library(tidyverse)
library(mverse)
# load and prepare data
soccer <- read.csv("data/soccer.csv")
soccer <- mutate(soccer, skintone = (rater1 + rater2) / 2)
# define position branch
new_position <- mutate_branch(
  position, # use position as is
  forcats::fct_collapse(
    position, 
    Forward = c("Center Forward", "Right Winger", "Left Winger"), 
    Midfielder = c("Defensive Midfielder", "Attacking Midfielder", 
                   "Right Midfielder", "Center Midfielder", "Left Midfielder"), 
    Defender = c("Center Back", "Right Fullback", "Left Fullback"), 
    Goalkeeper = c("Goalkeeper")), # groups position into 4 groups
  if_else(position == "Goalkeeper", "Goalkeeper", "Fielder") # group all fielders
)
# define size branch
size <- mutate_branch(height, weight)
# define model formula branch
# variable inclusion and exclusion are defined here
frmls <- formula_branch(
  cbind(redCards, games - redCards) ~ skintone + size,
  cbind(redCards, games - redCards) ~ skintone + size + new_position,
  cbind(redCards, games - redCards) ~ skintone + size + leagueCountry,
  cbind(redCards, games - redCards) ~ skintone + size + new_position + leagueCountry
)
# add branches
mv <- mverse(soccer) %>%
  add_mutate_branch(new_position, size) %>%
  add_formula_branch(frmls) 
multiverse_tree(mv, label = TRUE)
# define branch conditions to avoid redundancy
condition_position_1 <- branch_condition(
  cbind(redCards, games - redCards) ~ skintone + size,
  position)
condition_position_2 <- branch_condition(
  cbind(redCards, games - redCards) ~ skintone + size + leagueCountry,
  position)
mv <- mv %>%
  add_branch_condition(condition_position_1) %>%
  add_branch_condition(condition_position_2)
multiverse_tree(mv, label = TRUE)
# define and add family branch
fam <- family_branch("binomial")
add_family_branch(mv, fam)
glm_mverse(mv)
# multiverse_tree(mv, label = TRUE)
est <- summary(mv, conf.int = FALSE)
est %>%
  filter(term =="skintone") %>% 
  mutate(
    estimate.odd = exp(estimate),
    conf.low.odd = exp(estimate + qnorm(.025) * std.error),
    conf.high.odd = exp(estimate + qnorm(.975) * std.error)
  ) %>%
  select(estimate, estimate.odd, conf.low.odd, conf.high.odd, 
         new_position_branch, size_branch, frmls_branch)
# spec_curve(mv, "skintone", conf.int = FALSE)
