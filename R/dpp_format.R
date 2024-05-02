
dpp_format <- function(dpp_merged) {

 dpp_merged %>%
  transmute(

   # Predictors ---------------------------------------------------------------

   # Demographics ----

   age_yrs = agerand,


   treatment = factor(tolower(assign)),

   sex = factor(sex,
                levels = 1:2,
                labels = c("male",
                           "female")),

   education = edugrp,

   race_ethnicity = factor(race_eth,
                           levels = 1:4,
                           labels = c("caucasian",
                                      "african_american",
                                      "hispanic",
                                      "other")),

   # Body measurements ----

   weight_lbs = weight,

   bmi = bmi,

   # Labs ----

   insulin_fasting_uuml = i000,

   insulin_30m_uuml = i030,

   proinsulin_fasting_pm = pin,

   glucose_fasting_mgdl = g000,

   glucose_30m_mgdl = g030,

   glucose_2hr_mgdl = g120,

   chol_trig_mgdl = trig,

   chol_total_mgdl = chol,

   chol_hdl_mgdl = chdl,

   chol_ldlv_mgdl = vldl,

   chol_ldlc_mgdl = cldl,

   chol_ldlb_mgdl = ldlb,

   chol_ldlc_mgdl = ldlc,

   chol_ldl_particle_size = ldlz,

   chol_hdl_to_trig_ratio = chol_hdl_mgdl / chol_trig_mgdl,

   creatinine_urine_mgdl = ucre,

   hba1c_percent = hba1,

   fibrinogen_mgdl = fibr,

   crp_mgdl = crp,

   albumin_urine_mgdl = ualb,

   serum_ast_ul = sgot,

   serum_alt_ul = sgpt,

   serum_creatinine = crea,

   serum_bicarb = hco3,

   serum_potassium = k,

   serum_sodium = na,

   tpa_ngml = tpa,

   adiponectin = adipon,

   # dietary variables ----

   dt_kcal,

   dt_prot,

   dt_fat,

   dt_carb,

   dt_calc,

   dt_phos,

   dt_fe,

   dt_na,

   dt_pota,

   dt_a_iu,

   dt_a_re,

   dt_b1,

   dt_ribo,

   dt_niac,

   dt_vitc,

   dt_sfat,

   dt_olec,

   dt_lin,

   dt_chol,

   dt_dfib,

   dt_fol,

   dt_vite,

   dt_zinc,

   dt_anzn,

   dt_b6,

   dt_mg,

   dt_acar,

   dt_bcar,

   dt_cryp,

   dt_lut,

   dt_lyc,

   dt_ret,

   dt_proa,

   dt_star,

   dt_sucr,

   dt_galac,

   dt_gluc,

   dt_fruc,

   dt_lac,

   dt_pfa,

   dt_12_0,

   dt_14_0,

   dt_16_0,

   dt_18_0,

   dt_18_3,

   dt_20_5,

   dt_22_6,

   dt_tr_fa,

   # percentages based on diet calories

   percfat,

   perccarb,

   percprot,

   percsfat,

   percolec,

   perclin,

   percpfat,

   # alcohol ----

   servbeer,

   servwine,

   servliqu,

   alcbeer,

   alcwine,

   alcliqu,

   alc_day,

   # what are these? ----

   fg1,

   fg2,

   fg3,

   fg4,

   fg5,

   fg6,

   fg7,

   fg8,

   fg9,

   fg10,

   fg11,

   fg12,

   fg13,

   fg14,

   fg15,

   fg16,

   fg17,

   fg18,

   fg19,

   fg20,

   fg21,

   fg22,

   fg23,

   fg24,

   fg25,

   fg26,

   fg27,

   pfg1,

   pfg2,

   pfg3,

   pfg4,

   pfg5,

   pfg6,

   # what are these -----

   dt_star,

   dt_sucr,

   dt_galac,

   dt_gluc,

   dt_fruc,

   dt_lac,

   dt_pfa,

   dt_12_0,

   dt_14_0,

   dt_16_0,

   dt_18_0,

   dt_18_3,

   dt_20_5,

   dt_22_6,

   dt_tr_fa,

   # what are these ----

   percfat,

   perccarb,

   percprot,

   percsfat,

   percolec,

   perclin,

   percpfat,

   ws_bcar,

   ws_calc,

   ws_proa,

   ws_chol,

   ws_fat,

   ws_fol,

   ws_fe,

   ws_mg,

   ws_niac,

   ws_olec,

   ws_pfa,

   ws_phos,

   ws_pota,

   ws_a_re,

   ws_ret,

   ws_ribo,

   ws_b1,

   ws_b6,

   ws_a_iu,

   ws_vitc,

   ws_vite,

   ws_zinc,

   ws_14_0,

   ws_16_0,

   ws_20_5,

   ws_22_6,

   ws_18_0,

   ws_kcal,

   # these appear to be nominal (not sure its worth including)

   # addsalt,
   #
   # fatmeat,
   #
   # fatoil,
   #
   # fmealtm1,
   #
   # fmealtm2,
   #
   # howoften,
   #
   # iron,
   #
   # largmeal,
   #
   # leanmeat,
   #
   # lfbacon,
   #
   # lfcake,
   #
   # lfcheese,
   #
   # lfchips,
   #
   # lflunch,
   #
   # lfyogurt,
   #
   # mealsday,
   #
   # othervit,
   #
   # sevenalc,
   #
   # skinchic,
   #
   # snacks,
   #
   # vitamin,
   #
   # yeast,
   #
   # zinc,
   #
   # selenium,
   #
   # qwvstyp,
   #
   # qwvstwk,
   #
   # totalqwb,
   #
   # q9a,
   #
   # q9b,
   #
   # q9c,

   # derived variables --------------------------------------------------------

   homa_ir = i000 * g000 / 405,

   homa_beta = (360*i000) / (g000-63),

   # Outcomes -----------------------------------------------------------------

   time_death = if_else(death == 1,
                        true = deathdays / 365.25,
                        false = totaltim),

   status_death = death,

   time_diabetes = pmax(diabt, 1/365.25),

   status_diabetes = diabf,

   time_hpg = pmax(fasthypt, 1/365.25),

   status_hpg = fasthypf

  )

}
