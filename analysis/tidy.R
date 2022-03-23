library(tidyverse)
# load and prepare data
soccer <- read.csv("data/soccer.csv")
soccer <- mutate(soccer, skintone = (rater1 + rater2) / 2)
# define position groupings
soccer <- soccer %>%
  mutate(
    position_common = forcats::fct_collapse( 
      # forcats::fct_collapse() is a convenient helper function for regrouping factors
      position, 
      Forward = c("Center Forward", "Right Winger", "Left Winger"), 
      Midfielder = c("Defensive Midfielder", "Attacking Midfielder", 
                     "Right Midfielder", "Center Midfielder", "Left Midfielder"), 
      Defender = c("Center Back", "Right Fullback", "Left Fullback"), 
      Goalkeeper = c("Goalkeeper")), # groups position into 4 groups
    position_fld_vs_gk = if_else(position == "Goalkeeper", "Goalkeeper", "Fielder") # group all fielders
  )
# specify 16 models
model_1 <- formula(cbind(redCards, games - redCards) ~ skintone + position + weight)
model_2 <- formula(cbind(redCards, games - redCards) ~ skintone + position_common + weight)
model_3 <- formula(cbind(redCards, games - redCards) ~ skintone + position_fld_vs_gk + weight)
model_4 <- formula(cbind(redCards, games - redCards) ~ skintone + weight)

model_5 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position + weight)
model_6 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position_common + weight)
model_7 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position_fld_vs_gk + weight)
model_8 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + weight)

model_9 <- formula(cbind(redCards, games - redCards) ~ skintone + position + height)
model_10 <- formula(cbind(redCards, games - redCards) ~ skintone + position_common + height)
model_11 <- formula(cbind(redCards, games - redCards) ~ skintone + position_fld_vs_gk + height)
model_12 <- formula(cbind(redCards, games - redCards) ~ skintone + height)

model_13 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position + height)
model_14 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position_common + height)
model_15 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + position_fld_vs_gk + height)
model_16 <- formula(cbind(redCards, games - redCards) ~ skintone + leagueCountry + height)

# place the model definitions in a list and use lapply
models <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8,
               model_9, model_10, model_11, model_12, model_13, model_14, model_15, model_16)
multiverse_fit <- lapply(models, function(x) {
  glm(x, family = "binomial", data = soccer)
})

multiverse_res_log <- lapply(multiverse_fit, function(x) {
  broom::tidy(x, conf.int = FALSE) %>%
    filter(term == "skintone") %>%
    select(estimate)
})
multiverse_res <- lapply(multiverse_fit, function(x) {
  broom::tidy(x, exponentiate = TRUE, conf.int = TRUE, conf.level = .95) %>%
    filter(term == "skintone") %>%
    select(estimate, conf.low, conf.high) %>%
    rename(estimate.odd = estimate, conf.low.odd = conf.low, conf.high.odd = conf.high)
})
cbind(bind_rows(multiverse_res_log), bind_rows(multiverse_res))
