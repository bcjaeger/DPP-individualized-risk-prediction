
# TODO: redo table 1 using predicted best tx as stratifier in dpp
# TODO: re-read and clarify methods section
# TODO: NNT to prevent 1 incident DM using ILI only, MF only, and
#       using recommendation from model
# TODO: add 1 sentence description of cstat calib and nri

source("packages.R")
source("conflicts.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(future)
library(future.callr)
plan(callr)


manuscript_version <- 7

if(!dir.exists(glue("doc/manuscript-v{manuscript_version}"))){
  dir.create(glue("doc/manuscript-v{manuscript_version}"))
}


labels_tar <- tar_target(
  labels,
  list(

    variables = c(
      age_yrs = "Age",
      race_ethnicity = "Race/ethnicity",
      sex = "Sex",
      education = "Educational attainment",
      treatment = "Treatment group",
      glucose_fasting_mgdl = "Fasting glucose",
      homa_ir = "HOMA-Insulin Resistance",
      homa_beta = "HOMA-Beta cell function",
      hba1c_percent = "Glycated Hemoglobin",
      chol_trig_mgdl = "Triglycerides",
      chol_ldlc_mgdl = "Low-density lipoprotein cholesterol",
      chol_hdl_mgdl = "High-density lipoprotein cholesterol",
      bmi = "Body mass index",
      sbp = "Systolic blood pressure",
      dbp = "Diastolic blood pressure"
    ),

    units = c(
      age_yrs = "years",
      glucose_fasting_mgdl = "mg/dl",
      hba1c_percent = "%",
      chol_trig_mgdl = "mg/dl",
      bmi = "kg/m2",
      chol_ldlc_mgdl = "mg/dl",
      chol_hdl_mgdl = "mg/dl",
      sbp = "mm Hg",
      dbp = "mm Hg"
    ),

    levels = list(
      sex = c(
        "male" = "Male",
        "female" = "Female"
      ),

      race_ethnicity = c(
        "caucasian" = "Non-Hispanic White",
        "african_american" = "Non-Hispanic Black",
        "hispanic" = "Hispanic",
        "other" = "Other/Chinese"
      ),

      education = c(
        "less than high school" = "< High School",
        "high school" = "High School Graduate",
        "some college to college degree" = "Some College or College Graduate"
      )
    )
  )
)

# step 1: load all the components of DPP
dpp_components_tar <- tar_target(
  dpp_components,
  dpp_load(format = "\\.sas7bdat$")
)

# step 2: merge, keeping data collected PRIOR to randomization
dpp_merged_tar <- tar_target(
  dpp_merged,
  dpp_merge(dpp_components)
)

# step 3, rename variables and derive some
dpp_formatted_tar <- tar_target(
  dpp_formatted,
  dpp_format(dpp_merged)
)

dpp_excluded_tar <- tar_target(
  dpp_excluded,
  dpp_exclude(dpp_formatted)
)

mesa_init_tar <- tar_target(
  mesa_init,
  mesa_load()
)

mesa_excluded_tar <- tar_target(
  mesa_excluded,
  mesa_exclude(mesa_init)
)

tbl_exclusion_tar <- tar_target(
  tbl_exclusion,
  tabulate_exclusions(dpp_excluded, mesa_excluded)
)

tbl_characteristics_tar <- tar_target(
  tbl_characteristics,
  tabulate_characteristics(dpp_excluded, mesa_excluded, labels)
)

data_analysis_tar <- tar_target(
  data_analysis,
  bind_rows(DPP = dpp_excluded$included,
            MESA = mesa_excluded$included,
            .id = 'study')
)

analysis_tar <- tar_target(
  analysis,
  validate_preds(data_analysis)
)

reviewer_1_cstats_tar <- tar_target(
  reviewer_1_cstats,
  command = {

    data_list <- analysis[c("data_internal", "data_external")]

    cstats <- map(
      .x = data_list,
      .f = ~ {

        score_data <- select(.x, glucose_fasting_mgdl, hba1c_percent) %>%
          drop_na() %>%
          # mutate(across(everything(), impute_mean)) %>%
          as.list()

        rspec <- round_spec() %>%
          round_using_decimal(2)

        sc <- Score(score_data,
                    data = drop_na(.x, glucose_fasting_mgdl, hba1c_percent),
                    formula = Surv(time, status) ~ 1,
                    times = 3,
                    metrics = 'auc')

        sc$AUC$score %>%
          transmute(model,
                    auc = table_glue("{100*AUC} ({100*lower}, {100*upper})", rspec = rspec)) %>%
          pivot_wider(names_from = model, values_from = auc) %>%
          mutate(pval = sc$AUC$contrasts$p[1])
      }
    )
  }
)

tbl_reclass_tar <- tar_target(
  tbl_reclass,
  tabulate_reclassification(analysis)
)

tbl_optim_tar <- tar_target(
  tbl_optim,
  tabulate_optimal_tx(analysis)
)

tbl_characteristics_optim_tar <- tar_target(
  tbl_characteristics_optim,
  tabulate_characteristics_optim(data_analysis,
                                 labels, analysis)
)

tbl_cph_tar <- tar_target(
  tbl_cph,
  tabulate_cph(analysis, labels)
)

fig_cuminc_tar <- tar_target(
  fig_cuminc,
  visualize_cuminc(data_analysis)
)

tbl_cuminc_tar <- tar_target(
  tbl_cuminc,
  tabulate_cuminc(data_analysis)
)

fig_dcurve_tar <- tar_target(
  fig_dcurve,
  visualize_dcurve(analysis)
)

fig_calib_tar <- tar_map(
  values = tibble(type = c("internal", "external")),
  tar_target(fig_calib, visualize_calib(analysis,
                                        pred_horizon = 3,
                                        type = type))
)

manuscript_tar <- tar_render(
  manuscript,
  path = here::here("doc/manuscript.Rmd"),
  output_file = paste0("manuscript", "-v", manuscript_version, "/",
                       "manuscript-", basename(here()),
                       "-v", manuscript_version,
                       ".docx")
)

targets <- list(
  labels_tar,
  dpp_components_tar,
  dpp_merged_tar,
  dpp_formatted_tar,
  dpp_excluded_tar,
  mesa_init_tar,
  mesa_excluded_tar,
  tbl_exclusion_tar,
  tbl_characteristics_tar,
  tbl_characteristics_optim_tar,
  tbl_cph_tar,
  data_analysis_tar,
  analysis_tar,
  tbl_reclass_tar,
  tbl_optim_tar,
  fig_cuminc_tar,
  tbl_cuminc_tar,
  fig_dcurve_tar,
  fig_calib_tar,
  manuscript_tar,
  reviewer_1_cstats_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)
