# -------------------------------------------------------------------------
# File: model_functions.R
#
# Description:
# Custom defined functions for modeling in the *BE sat/stood*
# project
# -------------------------------------------------------------------------

FitModelDistVP <- function(data){
  data <- data %>%
    dplyr::filter(postmodifier_vp == "y") %>%
    mutate(variant = as.factor(variant) %>%
             relevel(ref = "ing"),
           postmodifier_vp = as.integer(postmodifier_vp) %>%
             as.factor())

  m <- glm(variant ~ verb * (dist_to_post_vp + subj_person), data = data, family = binomial)

  return(m)
}