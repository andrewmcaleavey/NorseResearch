#' SCORING using factor-standardized scoring system
#'
#' Ideally, this will use publically-available scoring data to score
#' standardized versions of each NF2.0 scale.
#'
#' STEPS:
#' 1. Identify optimal (graded response model) models for scoring each subscale in a way
#' that is consistent with clinical use and allows score computation with missing
#' item-level data.
#' 2. Include those models in this package
#' 3. Provide functions to compute factor scores from item-level data
#' 4. Compute factor scores from item-level data in the real and synthetic data.

require(NORSEpkg)
require(mirt)
require(NorseResearch)

# compute the models
grm.eating <- mirt::mirt(data = HF_research_data_2021 %>%
                           select(all_of(NORSEpkg::eating.names)),
                         model = 1,
                         itemtype = "graded")

eating.fscores <- fscores(grm.eating,
                          full.scores = TRUE,
                          full.scores.SE = TRUE) %>%
  as_tibble() %>%
  mutate(eating_fscore = F1,
         eating_fscore_SE = SE_F1,
         .keep = "none")
# cor(eating.fscores$eating_fscore, HF_research_data_2021$eating, use = "complete.obs")
# plot(eating.fscores$eating_fscore, HF_research_data_2021$eating)

get_fscores <- function(scale){
  varnames <- paste0(scale, ".names") %>%
    get(envir = .GlobalEnv)

  grm_obj <- mirt::mirt(data = HF_research_data_2021 %>%
                          filter(pt_order == 1) %>%
                          select(all_of({{ varnames }})),
                        model = 1,
                        itemtype = "graded")
  fscores_options <- mirt::fscores(grm_obj,
                                   full.scores = FALSE)
  fscores_obj <- HF_research_data_2021 %>%
    left_join(data.frame(fscores_options)) %>%
    select(F1, SE_F1)

  names(fscores_obj) <- paste0(scale, "_", c("fscore", "fscore_se"))
  fscores_obj
}

cog.fscores          <- get_fscores("cog")
control.fscores      <- get_fscores("control")
eating.fscores       <- get_fscores("eating")
genFunc.fscores      <- get_fscores("genFunc")
hopeless.fscores     <- get_fscores("hopeless")
internal.fscores     <- get_fscores("internal")
irritable.fscores    <- get_fscores("irritable")
ready.fscores        <- get_fscores("ready")
recovEnv.fscores     <- get_fscores("recovEnv")
sad.fscores          <- get_fscores("sad")
selfCrit.fscores     <- get_fscores("selfCrit")
avoidSit.fscores     <- get_fscores("avoidSit")
avoidSoc.fscores     <- get_fscores("avoidSoc")
socialSafety.fscores <- get_fscores("socialSafety")
somAnx.fscores       <- get_fscores("somAnx")
subRecov.fscores     <- get_fscores("subRecov")
subUse.fscores       <- get_fscores("subUse")
suicide.fscores      <- get_fscores("suicide")
trauma.fscores       <- get_fscores("trauma")
worry.fscores        <- get_fscores("worry")

HF_research_data_2021_fscores <- HF_research_data_2021 %>%
  bind_cols(cog.fscores         ,
            control.fscores     ,
            eating.fscores      ,
            genFunc.fscores     ,
            hopeless.fscores    ,
            internal.fscores    ,
            irritable.fscores   ,
            ready.fscores       ,
            recovEnv.fscores    ,
            sad.fscores         ,
            selfCrit.fscores    ,
            avoidSit.fscores    ,
            avoidSoc.fscores    ,
            socialSafety.fscores,
            somAnx.fscores      ,
            subRecov.fscores    ,
            subUse.fscores      ,
            suicide.fscores     ,
            trauma.fscores      ,
            worry.fscores
  ) %>%
  group_by(anon_id) %>%
  mutate(cog_fs_first_pt = first(cog_fscore, order_by = date),
         control_fs_first_pt = first(control_fscore, order_by = date),
         eating_fs_first_pt = first(eating_fscore, order_by = date),
         genFunc_fs_first_pt = first(genFunc_fscore, order_by = date),
         hopeless_fs_first_pt = first(hopeless_fscore, order_by = date),
         internal_fs_first_pt = first(internal_fscore, order_by = date),
         irritable_fs_first_pt = first(irritable_fscore, order_by = date),
         ready_fs_first_pt = first(ready_fscore, order_by = date),
         recovEnv_fs_first_pt = first(recovEnv_fscore, order_by = date),
         sad_fs_first_pt = first(sad_fscore, order_by = date),
         selfCrit_fs_first_pt = first(selfCrit_fscore, order_by = date),
         avoidSit_fs_first_pt = first(avoidSit_fscore, order_by = date),
         avoidSoc_fs_first_pt = first(avoidSoc_fscore, order_by = date),
         socialSafety_fs_first_pt = first(socialSafety_fscore, order_by = date),
         somAnx_fs_first_pt = first(somAnx_fscore, order_by = date),
         subRecov_fs_first_pt = first(subRecov_fscore, order_by = date),
         subUse_fs_first_pt = first(subUse_fscore, order_by = date),
         suicide_fs_first_pt = first(suicide_fscore, order_by = date),
         trauma_fs_first_pt = first(trauma_fscore, order_by = date),
         worry_fs_first_pt = first(worry_fscore, order_by = date)) %>%
  ungroup()
usethis::use_data(HF_research_data_2021_fscores, overwrite = TRUE)
