#------------------------------------------------------------------------------#

# Helper functions for estimating children eligible for child care supports

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Functions to Assist Census Data Pulls and Processing -------------------------
#------------------------------------------------------------------------------#

# FORMULAS PROVIDED BY CENSUS (IGNORE COVARIANCE BETWEEN COMPONENT RANDOM VARIABLES)

# Source: https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2015StatisticalTesting5year.pdf,
#         https://www.census.gov/content/dam/Census/library/publications/2008/acs/ACSGeneralHandbook.pdf

    # "All methods in this section are approximations and users should be cautious in using them.
    # This is because these methods do not consider the correlation or covariance between the basic
    # estimates. They may be overestimates or underestimates of the derived estimates standard error,
    # depending on whether the two basic estimates are highly correlated in either the positive or
    # negative direction."

    # Census defines margin of error as 1.64 * standard error (10% confidence interval band). Divide by 1.64
    # to convert margin of error to standard error in Census/ACS data.

# FORMULAS DERIVED BY DELTA METHOD (ACCOUNT FOR COVARIANCE BETWEEN COMPONENT RANDOM VARIABLES)

# Source: http://statweb.stanford.edu/~susan/courses/s200/lectures/lect5.pdf

    # Here, we use the delta method to derive the formula for the variance of a proportion ((taking the variance of the Taylor series expansion)): 
    # var(A/B) = [d(A/B)/dA]^2 * var(A) + [d(A/B)/dB]^2 * var(B) + d(A/B)/dA * d(A/B)/dB * cov(A,B)
    #          = [1/B]^2 * var(A) + [-A/B^2]^2 *  var(B)  + [1/B] * [-A/B^2] * cov(A, B)
    #          = [1/B]^2 * var(A) + [-A/B^2]^2 *  var(B)  + [1/B] * [-A/B^2] * var(A) ((since B = A + not A))
    #          = [1/B]^2 * [[1-[A/B]] * var(A) + [A/B]^2 * var(B)]

#--------------------------------------------------------------------

# STANDARD ERROR FOR AGGREGATED COUNT DATA (COLLAPSE)

    # Aggregate over the set of variables specified in "by" statement.
    # This is only an approximation of the true SE, since we do not observe the covariance between
    # the components of the sum in the published ACS data (requires unpublished microdata).

se_sum <- function(moe) {
  return(sqrt(sum((moe/1.645)^2, na.rm = TRUE)))
}

# STANDARD ERROR FOR PRODUCT OF RANDOM VARIABLES


se_product <- function(A, B, A_se, B_se) {

  # Using the delta-method formula
  ## var(A*B) = [d(A*B)/dA]^2 * var(A) + [d(A*B)/dB]^2 * var(B) + 2 * d(A*B)/dA * d(A*B)/dB * cov(A,B)
  ##          = [B]^2 * var(A) + [A]^2 *  var(B)  + 2 * [AB] * cov(A, B)
    
  # Note -- this assumes zero covariance
  sqrt(A^2*B_se^2 + B^2*A_se^2)
}

# STANDARD ERROR FOR DERIVED PROPORTION

  # See derivation using Delta Method, above.

se_proportion <- function(A, B, se_A, se_B) {
  return((1/B)*sqrt( (1-(A/B))*(se_A)^2 + (A/B)^2*(se_B)^2) )
}

# STANDARD ERROR FOR (NON-PROPORTION) RATIO

  # This is only an approximation of the true SE, since we do not observe the covariance between the components of the
  # ratio in the published ACS data.

se_ratio <- function(A, B, se_A, se_B) {
  return((1/B)*sqrt( (se_A)^2 + (A/B)^2*(se_B)^2) )
}

# SUM OF STANDARD ERRORS

se_col_sum <- function(...) {
  args <- list(...)
  args_2 <- lapply(args, function(x) x^2)
  Reduce(`+`, args_2) %>% sqrt()
}

# Test this function
if (FALSE) {
  with(mtcars, se_col_sum(mpg, hp, wt))
}

#------------------------------------------------------------------------------#
# Functions to categorize data -------------------------------------------------
#------------------------------------------------------------------------------#

# /!\ Note: could use cut() as an alternative to the case_whens

# Recode age ------------------------------------------------------------------#

bin_age <- function(a) {
  case_when(between(a,  0,  2) ~ "0to2", 
            between(a,  3,  5) ~ "3to5", 
            #between(a,  5,  5) ~ "5to5", 
            between(a,  6,  8) ~ "6to8", 
            between(a,  9, 12) ~ "9to12", 
            between(a, 13, 14) ~ "13to14",
            between(a, 15, 17) ~ "15to17") 
}

# Recode poverty decimal ------------------------------------------------------#

bin_incpov_ratio <- function(incpov_ratio, pov_breaks = seq(0, 3, by = 0.5)) {
  
  # Bin values of income-to-poverty ratios
  pov_breaks_aug <- c(pov_breaks, Inf)
  pov_labels <- paste0(percent(pov_breaks_aug[-length(pov_breaks_aug)]), 
                       "-", 
                       percent(pov_breaks_aug[-1]))
  pov_labels[length(pov_labels)] <- 
    pov_breaks[length(pov_breaks_aug)-1] %>% 
    percent() %>% 
    paste0("+")
  
  pov_cuts <- cut(incpov_ratio, 
                  breaks = pov_breaks_aug,
                  labels = pov_labels)
  return(pov_cuts)
}


# Recode industry codes -------------------------------------------------------#

bin_industry <- function(i) {
  case_when(between(i, 0170, 0490) ~ "ag",
            between(i, 0770, 0770) ~ "constr",
            between(i, 1070, 3990) ~ "manuf",
            between(i, 4070, 4590) ~ "whtrade",
            between(i, 4760, 5790) ~ "retail_trade",
            between(i, 6070, 6390) ~ "transport",
            between(i, 0570, 0690) ~ "transport",
            between(i, 6470, 6780) ~ "info",
            between(i, 6870, 7190) ~ "finance",
            between(i, 7270, 7790) ~ "profscimgmt_services",
            between(i, 7860, 8470) ~ "educhealthsoc_services",
            between(i, 8561, 8690) ~ "artsfood_services",
            between(i, 8770, 9290) ~ "other_services",
            between(i, 9370, 9590) ~ "public",
            between(i, 9670, 9870) ~ "military",
            TRUE ~ "NA")
}

# Recode CPS FAMINC field as poverty interval ---------------------------------#

# Randomly develop income-to-poverty data for Current Population Survey records
# given values of the `faminc` field, which is binned without alignment to 
# common poverty thresholds.

recode_intervals <- function(x) str_replace_all(x, "\\(|\\)|\\[|\\]", "") %>% str_replace_all(",", "-")
draws <- runif(10, 0, 4)
cbind(draws, cut(draws, breaks = c(seq(0, 10, by = 0.5))) %>% as.character() %>% fct_relabel(recode_intervals) %>% as.character())

faminc_bounds <- tribble(~faminc, ~faminc_lb, ~faminc_ub,
                         100,      0,   4999,
                         210,   5000,   7499,
                         300,   7500,   9999,
                         430,  10000,  12499,
                         470,  12500,  14999,
                         500,  15000,  19999,
                         600,  20000,  24999,
                         710,  25000,  29999,
                         720,  30000,  34999,
                         730,  35000,  39999,
                         740,  40000,  49999,
                         820,  50000,  59999,
                         830,  60000,  74999,
                         841,  75000,  99999,
                         842, 100000, 149999,
                         843, 150000, 150000,
                         )

# Calculate income-to-pov ratios based on federal poverty line estimates ------#

# Source for 2018: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2018-poverty-guidelines/2018-poverty-guidelines-computations
# Source for 2019: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2019-poverty-guidelines/2019-poverty-guidelines-computations-page
# Source for 2020: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2020-poverty-guidelines/2020-poverty-guidelines-computations
# Source for 2021: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines/2021-poverty-guidelines-computations
# Source for 2022: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
fpl_by_year <- 
  tribble(~fpl_fam_size, ~fpl_2018, ~fpl_2019, ~fpl_2020, ~fpl_2021, ~fpl_2022,
                      1,     12140,     12490,    12760,      12880,     13590,
                      2,     16460,     16910,    17240,      17420,     18310,
                      3,     20780,     21330,    21720,      21960,     23030,
                      4,     25100,     25750,    26200,      26500,     27750,
                      5,     29420,     30170,    30680,      31040,     32470,
                      6,     33740,     34590,    35160,      35580,     37190,
                      7,     38060,     39010,    39640,      40120,     41910,
                      8,     42380,     43430,    44120,      44660,     46630)

# Augment the table to generate FPL values for families up to size 20
fpl_by_year_fam_size7 <- fpl_by_year %>% filter(fpl_fam_size == 7) %>% dplyr::select(-fpl_fam_size)
fpl_by_year_fam_size8 <- fpl_by_year %>% filter(fpl_fam_size == 8) %>% dplyr::select(-fpl_fam_size)
fpl_by_year_diff <- fpl_by_year_fam_size8 - fpl_by_year_fam_size7

fpl_by_year_aug <- fpl_by_year
for (x in 1:12) {
  fpl_by_year_aug <- 
    bind_rows(fpl_by_year_aug,
              (fpl_by_year_diff*x + fpl_by_year_fam_size8) %>% 
                mutate(fpl_fam_size = 8 + x))
}

fredr_set_key(fred_key)
cpi <- 
  fredr(
    series_id = "CPIAUCSL",
    observation_start = as.Date("1998-01-01"),
    observation_end   = Sys.Date()) %>% 
  mutate(fpl_year = year(date),
         fpl_month = month(date)) %>% 
  group_by(fpl_year) %>% 
  mutate(ytd_infl = value / value[1])

fpl_by_month <- 
  fpl_by_year_aug %>% 
  pivot_longer(cols = -fpl_fam_size,
               names_to = "fpl_year",
               values_to = "fpl") %>% 
  mutate(fpl_year = str_replace(fpl_year, "fpl_", "") %>% as.numeric()) %>% 
  merge(cpi,
        by = "fpl_year") %>% 
  mutate(fpl = fpl * ytd_infl) %>% 
  dplyr::select(fpl_year, fpl_month, fpl_fam_size, ytd_infl, fpl) %>% 
  data.table()

if (FALSE) {
  fpl_by_month %>% 
    filter(fpl_fam_size %in% 1:8) %>% 
    mutate(y_m = fpl_year + (fpl_month - 1) / 12) %>% 
    ggplot(aes(x = y_m,
               y = fpl,
               color = factor(fpl_year))) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = comma) +
    facet_wrap(~fpl_fam_size,
               scale = "free")
}
  
# Note -- this is semi-deprecated, since it was easier to implement in the
# cps data-prep code by merging the fpl_by_month data directly into the
# data
get_pov_threshold <- function(fam_size, year, month) {
  
  my_fpl <- 
    fpl_by_month %>% 
    filter(fpl_fam_size == fam_size,
           fpl_year     == year,
           fpl_month    == month) %>% 
    pull(fpl)
  
  if (length(my_fpl) == 0) {
    return(NA)
  } else {
    return(my_fpl)
  }
}

if (FALSE) {
  old_fpl <- function(fam_size) {
    case_when(fam_size == 1 ~ 12880,
              fam_size == 2 ~ 17420,
              fam_size == 3 ~ 21960,
              fam_size == 4 ~ 26500,
              fam_size == 5 ~ 31040,
              fam_size == 6 ~ 35580,
              fam_size == 7 ~ 40120, 
              fam_size == 8 ~ 44660, 
              fam_size  > 8 ~ 44660 + (fam_size-8)*4540, 
              # Adding $4,540 for each additional person is documented in the 
              # ASPE poverty guideline
              # https://aspe.hhs.gov/sites/default/files/documents/d2eececdc1ca66dfd41ca1d2a524e076/HHS-Poverty-Guidelines-Fed-Register-2021.pdf 
              TRUE ~ NaN) 
  } 
  cbind(sapply(1:8, old_fpl),
        sapply(1:8, function(x) get_pov_threshold(x, year = base_year, month = 1)))
}

draw_inc <- function(faminc, fixed_results = TRUE) {
  # This function randomly draws income from FAMINC categories
  
  if (fixed_results) set.seed(60637)
  
  # Draw income randomly from the interval
  my_bounds <- faminc_bounds[match(faminc, faminc_bounds$faminc),]
  inc_draws <- with(my_bounds, runif(length(faminc), faminc_lb, faminc_ub))
  
  return(inc_draws)
}

calc_incpov_rat <- function(inc, famsize, year, month) {
  # This function take a continuous income measure as an input, and calculates
  # income-to-poverty line ratios
  
  my_pov_thresholds <- get_pov_threshold(famsize, year, month)
  my_incpov_ratio <- inc / my_pov_thresholds
  
  return(my_incpov_ratio)
}

draw_incpov_rat <- function(faminc, famsize, year, month, fixed_results = TRUE) {
  # This function both draws income from FAMINC categories, and
  # calculates income-to-poverty ratios
  
  # Draw income randomly from the FAMINC intervals
  inc_draws <- draw_inc(faminc, fixed_results = fixed_results)
  
  # Calculate ratio
  my_incpov_ratio <- calc_incpov_rat(inc_draws, famsize, year, month)
  
  return(my_incpov_ratio)
}

#------------------------------------------------------------------------------#
# Functions for controlling code runs and data output --------------------------
#------------------------------------------------------------------------------#

# A function to source only the code in an .Rmd file
# See this SO post for Yihui Xie's recommendation for this:
#   https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r
source_rmd <- function(x, ...) {
  library(knitr)
  source(purl(x, 
              output = tempfile()),
         ...)
}

# Function to display IPUMS documenttation
# See vignettes related to the `ipumsr` 
show_ipums_doc <- function(ddi_file, fields = NULL) {
  doc_file <- ipums_var_info(ddi_file)
  # Subset if specified
  if (!is.null(fields)) {
    doc_file <- filter(doc_file, 
                       var_name %in% fields)
  }
  # Retain only certain columns
  doc_file <- 
    doc_file %>% select(var_name, var_label, var_desc, val_labels)
  
  out <- NULL
  for (i in 1:nrow(doc_file)) {
    doc_file[i, ] %>% 
      with(paste(var_name, var_label, var_desc, 
                 sep = "\n")) %>% 
      cat()
    cat("\n\n")
    kable(doc_file$val_labels[i]) %>% 
      cat()
    cat("\n\n----------------\n\n")
  }
}


#------------------------------------------------------------------------------#
# Smaller modeling functions ---------------------------------------------------
#------------------------------------------------------------------------------#

build_fm <- function(lhs, rhs) {
  # lhs -- single character value
  # rhs -- vector of multiple controls
  paste0(lhs, " ~ ",
         paste(rhs, collapse = " + ")) %>% 
    as.formula()
}

#------------------------------------------------------------------------------#
# Function for producing field labels ------------------------------------------
#------------------------------------------------------------------------------#

label_outcome <- function(outcome_var) {
  
  sapply(outcome_var,
         function(x) {
           switch(x,
                  "deep_pov_post"     = "Deep Poverty (unadj)",
                  "hs_elig_post"      = "HS Eligible (unadj)",
                  "pfa_elig_post"     = "PFA Eligible (unadj)",
                  "deep_pov_adj"      = "Deep Poverty",
                  "hs_elig_post_adj"  = "HS Eligible",
                  "pfa_elig_post_adj" = "PFA Eligible",
                  "hoh_empl_post"     = "HOH Employed in Post",
                  "ccdf_elig_tight_inc"     = "CCAP Elig 'tight' (unadj)",
                  "ccdf_elig_tight_inc_adj" = "CCAP Elig 'tight' (adj)",
                  "ccdf_elig_loose_inc"     = "CCAP Elig 'loose' (unadj)",
                  "ccdf_elig_loose_inc_adj" = "CCAP Elig 'loose' (adj)")
         })
}

label_vars <- function(var) {
  
  sapply(var,
         function(x) {
           switch(x,
                  "ed_hs"       = "Educ - HS",
                  "ed_somecoll" = "Educ - Some Coll",
                  "ed_coll"     = "Educ - Coll",
                  "share_0_50"    = "Inc-to-Pov: 0-50%",
                  "share_50_100"  = "Inc-to-Pov: 50-100%",
                  "share_0_100"   = "Inc-to-Pov: 0-100%",
                  "share_100_200" = "Inc-to-Pov: 100-200%",
                  "share_200_300" = "Inc-to-Pov: 200-300%",
                  "share_300"     = "Inc-to-Pov: 300%+",
                  "share_WorkElig_0_50"    = "HH Working, Inc-to-Pov: 0-50%",
                  "share_WorkElig_0_75"    = "HH Working, Inc-to-Pov: 0-75%",
                  "share_WorkElig_50_100"  = "HH Working, Inc-to-Pov: 50-100%",
                  "share_WorkElig_75_150"  = "HH Working, Inc-to-Pov: 75-150%",
                  "share_WorkElig_0_100"   = "HH Working, Inc-to-Pov: 0-100%",
                  "share_WorkElig_100_200" = "HH Working, Inc-to-Pov: 100-200%",
                  "share_WorkElig_150_225" = "HH Working, Inc-to-Pov: 150-225%",
                  "share_WorkElig_200_300" = "HH Working, Inc-to-Pov: 200-300%",
                  "share_WorkElig_225_300" = "HH Working, Inc-to-Pov: 225-300%",
                  "share_WorkElig_300"     = "HH Working, Inc-to-Pov: 300%+",
                  "share_NotWorkElig_0_50"    = "HH Not Working, Inc-to-Pov: 0-50%",
                  "share_NotWorkElig_0_75"    = "HH Not Working, Inc-to-Pov: 0-75%",
                  "share_NotWorkElig_50_100"  = "HH Not Working, Inc-to-Pov: 50-100%",
                  "share_NotWorkElig_75_150"  = "HH Not Working, Inc-to-Pov: 75-150%",
                  "share_NotWorkElig_0_100"   = "HH Not Working, Inc-to-Pov: 0-100%",
                  "share_NotWorkElig_100_200" = "HH Not Working, Inc-to-Pov: 100-200%",
                  "share_NotWorkElig_150_225" = "HH Not Working, Inc-to-Pov: 150-225%",
                  "share_NotWorkElig_200_300" = "HH Not Working, Inc-to-Pov: 200-300%",
                  "share_NotWorkElig_225_300" = "HH Not Working, Inc-to-Pov: 225-300%",
                  "share_NotWorkElig_300"     = "HH Not Working, Inc-to-Pov: 300%+",
                  
                  "share_NotWorkElig_SpousePresent"   = "HH Not Working, Spouse Present",
                  "share_WorkElig_SpousePresent"      = "HH Working, Spouse Present",
                  "share_NotWorkElig_NoSpousePresent" = "HH Not Working, No Spouse Present",
                  "share_WorkElig_NoSpousePresent"    = "HH Working, No Spouse Present",
                  
                  "incpov_r0to50_est"     = "Inc-to-Pov: 0-50%",
                  "incpov_r50to100_est"   = "Inc-to-Pov: 50-100%",
                  "incpov_r100to199_est"  = "Inc-to-Pov: 100-199%",
                  "incpov_r200to299_est"  = "Inc-to-Pov: 200-299%",
                  "f_lesshs_est"   = "Educ < HS, Female",
                  "m_lesshs_est"   = "Educ < HS, Male",
                  "f_hsgrad_est"   = "Educ - HS, Female",
                  "m_hsgrad_est"   = "Educ - HS, Male",
                  "f_somecoll_est" = "Educ - Some Coll, Female",
                  "m_somecoll_est" = "Educ - Some Coll, Male",
                  "f_coll_est" = "Educ - Coll, Female",
                  "m_coll_est" = "Educ - Coll, Male",
                  "pctMale_noSp_est"   = "Household is Male-Headed Only",
                  "pctFemale_noSp_est" = "Household is Female-Headed only",
                  "pctMarried_est"     = "Household is Married Couple",
                  "employrate_m_a2534_est" = "Empl't Rate, Male Age 25-34",
                  "employrate_f_a2534_est" = "Empl't Rate, Female Age 25-34",
                  "employrate_m_est"       = "Empl't Rate, Male",
                  "employrate_f_est"       = "Empl't Rate, Female",
                  "lfrate_m_a2534_est"     = "LF Part Rate, Male Age 25-34",
                  "lfrate_f_a2534_est"     = "LF Part Rate, Female Age 25-34",
                  "lfrate_m_est"           = "LF Part Rate, Male",
                  "lfrate_f_est"           = "LF Part Rate, Female")
         })
}



