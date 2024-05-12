#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param visit
mesa_load <- function() {

  fpath <- file.path(
    "\\\\medctr.ad.wfubmc.edu",
    "dfs",
    "phs_research$",
    "bancks_2022ada",
    "bancks",
    "MESA",
    "Analytic data"
  )

  mesa_visit_one <-
    read_sas(file.path(fpath, "mesaex1_nodiabv20231128.sas7bdat")) %>%
    transmute(
      id = idno,
      prediabetes = prediabex1,
      visit = 1L,
      time_diabetes = exdiabpyr,
      status_diabetes = incdiab,
      time_death = deathpyr,
      status_death = death2019,
      age_yrs = age1c,
      sex = factor(female, levels = c(0,1), labels = c("male", "female")),
      race_ethnicity = factor(race1c,
                              labels = c("caucasian",
                                         "african_american",
                                         "hispanic",
                                         "other")),
      education = factor(edugrp,
                         levels = c(0, 1, 2, 3),
                         labels = c("less than high school",
                                    "high school",
                                    "some college to college degree",
                                    "some college to college degree")),
      treatment = factor(NA,
                         levels = c("lifestyle",
                                    "metformin",
                                    "placebo")),
      glucose_fasting_mgdl = glucos1c,
      hba1c_percent = NA_real_,
      chol_ldlv_mgdl = ldl1,
      chol_trig_mgdl = trig1,
      bmi = bmi1c
    )

  mesa_visit_two <-
    read_sas(file.path(fpath, "mesaex2_nodiab.sas7bdat")) %>%
    transmute(
      id = idno,
      prediabetes = glucos2c >= 100 &
        glucos2c <= 125 &
        hba1c2 < 6.5    &
        hba1c2 > 5.7,
      visit = 2L,
      time_diabetes = exdiabpyr26,
      status_diabetes = incdiab26,
      time_death = deathpyr26,
      status_death = death2019,
      age_yrs = age2c,
      sex = factor(female, levels = c(0,1), labels = c("male", "female")),
      race_ethnicity = factor(race1c,
                              labels = c("caucasian",
                                         "other",
                                         "african_american",
                                         "hispanic")),
      treatment = factor(NA,
                         levels = c("lifestyle",
                                    "metformin",
                                    "placebo")),
      glucose_fasting_mgdl = glucos2c,
      hba1c_percent = hba1c2,
      chol_ldlv_mgdl = ldl2,
      chol_trig_mgdl = trig2,
      bmi = bmi2c
    )

  bind_rows(mesa_visit_one,
            mesa_visit_two) %>%
    drop_na(time_diabetes, status_diabetes) %>%
    arrange(id, visit)

}
