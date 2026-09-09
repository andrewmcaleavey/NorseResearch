## Build the package's internal data store.
##
## Run this after the other data-raw scripts. Objects used by package code are
## kept in R/sysdata.rda so they are available in the package namespace when
## users call requireNamespace("NorseResearch"), while the corresponding data
## files remain available to users who attach the package.

internal_objects <- c(
  "item_names_nf2", "nf2.1.item.descriptions", "nf2.1.logic", "NF3.1_items",
  "summary_norms_MH_out", "alliance.names", "alliance.names.nf3",
  "anger.names.nf3", "avoidSit.names", "avoidSoc.names", "cog.names",
  "cog.names.nf3", "control.names", "eating.names", "eating.names.nf3",
  "genFunc.names", "genFunc.names.nf3", "hopeless.names", "hopeless.names.nf3",
  "impulsivity.names.nf3", "internal.names", "intAvoid.names.nf3",
  "intMem.names.nf3", "irritable.names", "names.list", "needs.names",
  "ona.names", "ona.names.nf3", "pref.names.nf3", "nicer_names_nf3",
  "nicer.nf2.names", "pain.names.nf3", "physAnx.names.nf3", "ready.names",
  "ready.names.nf3", "recovEnv.names", "sad.names", "sad.names.nf3",
  "scale_names", "scale_names_nf3", "scale_names_ou", "SDH.names.nf3",
  "selfComp.names.nf3", "selfContempt.names.nf3", "selfCrit.names",
  "single.items.names", "single.items.names.nf3", "socAvoid.names.nf3",
  "socialSafety.names", "socSup.names.nf3", "somAnx.names", "subRecov.names",
  "subUse.names", "subUse.names.nf3", "suicide.names", "suicide.names.nf3",
  "trauma.names", "worry.names", "worry.names.nf3", "scoreNames.nf3"
)

source_env <- new.env(parent = emptyenv())
for (object in internal_objects) {
  load(file.path("data", paste0(object, ".rda")), envir = source_env)
}

save(
  list = internal_objects,
  file = "R/sysdata.rda",
  envir = source_env,
  version = 2
)
