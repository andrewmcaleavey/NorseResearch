## code to prepare `scoring_objects_raw` dataset goes here

# objects for scoring
#
# Creates several vectors of variable names that I will need

cog.names <- c("Q142", "Q143", "Q144", "Q145", "Q146", "Q147")
control.names <- c("Q26", "Q20", "Q68", "Q130")
eating.names <- c("Q46", "Q104", "Q57", "Q18", "Q63")
genFunc.names <- c("Q27", "Q140", "Q141")
hopeless.names <- c("Q15", "Q61", "Q115", "Q24", "Q88")
internal.names <- c("Q10", "Q34", "Q78", "Q122", "Q123")
irritable.names <- c("Q37", "Q114", "Q65")
ready.names <- c("Q133", "Q134", "Q135")
recovEnv.names <- c("Q80", "Q136", "Q138", "Q139", "Q137")
sad.names <- c("Q42", "Q102", "Q100")
selfCrit.names <- c("Q67", "Q38", "Q126", "Q124", "Q129", "Q127", "Q128", "Q101")
avoidSit.names <- c("Q119", "Q17", "Q118")
avoidSoc.names <- c("Q64", "Q120", "Q121")
socialSafety.names <- c("Q43", "Q131", "Q40", "Q132", "Q62", "Q50")
somAnx.names <- c("Q51", "Q103", "Q75", "Q3", "Q53")
subRecov.names <- c("Q109", "Q154", "Q108", "Q155")
subUse.names <- c("Q59", "Q4", "Q107", "Q35")
suicide.names <- c("Q19", "Q105", "Q77", "Q106")
trauma.names <- c("Q111", "Q110", "Q113", "Q112")
worry.names <- c("Q39", "Q116", "Q117")
alliance.names <- c("Q11", "Q12", "Q13", "Q14")
needs.names <- c("Q71", "Q72", "Q74", "Q152", "Q153")
single.items.names <- c("Q84", "Q148", "Q149", "Q150", "Q151")

names.list <- list(cog.names, control.names, eating.names, genFunc.names, hopeless.names,
                   internal.names, irritable.names, ready.names, recovEnv.names, sad.names,
                   selfCrit.names,  avoidSit.names, avoidSoc.names, socialSafety.names,
                   somAnx.names, subRecov.names, subUse.names, suicide.names, trauma.names,
                   worry.names, alliance.names, needs.names)

scale_names <- c("cog", "control", "eating", "genFunc", "hopeless", "internal",
                 "irritable", "ready", "recovEnv", "sad", "selfCrit", "avoidSit",
                 "avoidSoc", "socialSafety", "somAnx", "subRecov", "subUse",
                 "suicide", "trauma", "worry")

scale_names_ou <- paste0(scale_names, "_ou")

nicer.nf2.names <- c("Cognitive Problems", "Need for Control", "Eating Problems",
                     "General Functioning", "Hopelessness", "Internal Avoidance",
                     "Irritability", "Readiness for Recovery", "Recovery Environment",
                     "Sad Affect", "Self-Criticism", "Situational Avoidance",
                     "Social Avoidance", "Social Safety", "Somatic Anxiety",
                     "Substance Recovery", "Substance Use",
                     "Suicidality", "Trauma Reaction", "Worry")

scale_names_nf3 <- c("sad", "physAnx", "eating", "suicide", "subUse",
                     "intMem", "anger", "hopeless", "worry", "selfComp",
                     "socAvoid", "intAvoid", "selfContempt", "pain",
                     "socSup", "genFunc", "cog", "impulsivity", "ready",
                     "alliance", "pref")

nicer_names_nf3 <- c("Sad Affect", "Physical Anxiety", "Restrictive Eating",
                     "Suicidal Thoughts", "Substance Use", "Intrusive Memories",
                     "Anger", "Hopelessness", "Worry", "Self-Compassion",
                     "Social Avoidance", "Internal Avoidance", "Self-Contempt",
                     "Pain", "Social Support", "General Functioning",
                     "Cognitive Problems", "Impulsivity", "Readiness for Change",
                     "Alliance", "Therapy Preferences")

cog.names.nf3 <- c("Q145", "Q142", "Q143", "Q144", "Q147")
anger.names.nf3 <- c("Q201", "Q238", "Q218", "Q206")
eating.names.nf3 <- c("Q63", "Q104", "Q57", "Q18", "Q46")
genFunc.names.nf3 <- c("Q140", "Q207", "Q217", "Q220")
hopeless.names.nf3 <- c("Q115", "Q61", "Q24", "Q88")
intAvoid.names.nf3 <- c("Q34", "Q78", "Q122", "Q123")
ready.names.nf3 <- c("Q233", "Q231", "Q232", "Q234")
sad.names.nf3 <- c("Q100", "Q102", "Q42")
selfContempt.names.nf3 <- c("Q128", "Q126", "Q127", "Q101")
socAvoid.names.nf3 <- c("Q64", "Q120", "Q121", "Q118")
socSup.names.nf3 <- c("Q204", "Q43", "Q212", "Q222", "Q221")
physAnx.names.nf3 <- c("Q51", "Q103", "Q75", "Q53", "Q213")
subUse.names.nf3 <- c("Q59", "Q4", "Q107", "Q35")
suicide.names.nf3 <- c("Q19", "Q105", "Q77", "Q106")
intMem.names.nf3 <- c("Q111", "Q110", "Q112")
worry.names.nf3 <- c("Q117", "Q39", "Q208", "Q214")
impulsivity.names.nf3 <- c("Q205", "Q151", "Q223", "Q224")
pain.names.nf3 <- c("Q203", "Q210", "Q216", "Q219")
selfComp.names.nf3 <- c("Q202", "Q209", "Q211", "Q215")
impulsivity.names.nf3 <- c("Q205", "Q151", "Q223", "Q224")
impulsivity.names.nf3 <- c("Q205", "Q151", "Q223", "Q224")

single.items.names.nf3 <- c("Q84", "Q83", "Q148", "Q149", "Q150", "Q225")
SDH.names.nf3 <- c("Q230", "Q227", "Q228", 'Q229')
QOL.name.nf3 <- "Q226"

alliance.names.nf3 <- c("Q236", "Q235", "Q237")
pref.names.nf3 <- c("Q71", "Q74", "Q152", "Q153")


# use_data() calls for everything that contains "names"

usethis::use_data(alliance.names, overwrite = TRUE)
usethis::use_data(alliance.names.nf3, overwrite = TRUE)
usethis::use_data(anger.names.nf3, overwrite = TRUE)
usethis::use_data(avoidSit.names, overwrite = TRUE)
usethis::use_data(avoidSoc.names, overwrite = TRUE)
usethis::use_data(cog.names, overwrite = TRUE)
usethis::use_data(cog.names.nf3, overwrite = TRUE)
usethis::use_data(control.names, overwrite = TRUE)
usethis::use_data(eating.names, overwrite = TRUE)
usethis::use_data(eating.names.nf3, overwrite = TRUE)
usethis::use_data(genFunc.names, overwrite = TRUE)
usethis::use_data(genFunc.names.nf3, overwrite = TRUE)
usethis::use_data(hopeless.names, overwrite = TRUE)
usethis::use_data(hopeless.names.nf3, overwrite = TRUE)
usethis::use_data(impulsivity.names.nf3, overwrite = TRUE)
usethis::use_data(internal.names, overwrite = TRUE)
usethis::use_data(intAvoid.names.nf3, overwrite = TRUE)
usethis::use_data(intMem.names.nf3, overwrite = TRUE)
usethis::use_data(irritable.names, overwrite = TRUE)
usethis::use_data(names.list, overwrite = TRUE)
usethis::use_data(needs.names, overwrite = TRUE)
usethis::use_data(pref.names.nf3, overwrite = TRUE)
usethis::use_data(nicer_names_nf3, overwrite = TRUE)
usethis::use_data(nicer.nf2.names, overwrite = TRUE)
usethis::use_data(pain.names.nf3, overwrite = TRUE)
usethis::use_data(physAnx.names.nf3, overwrite = TRUE)
usethis::use_data(ready.names, overwrite = TRUE)
usethis::use_data(ready.names.nf3, overwrite = TRUE)
usethis::use_data(recovEnv.names, overwrite = TRUE)
usethis::use_data(sad.names, overwrite = TRUE)
usethis::use_data(sad.names.nf3, overwrite = TRUE)
usethis::use_data(scale_names, overwrite = TRUE)
usethis::use_data(scale_names_nf3, overwrite = TRUE)
usethis::use_data(scale_names_ou, overwrite = TRUE)
usethis::use_data(SDH.names.nf3, overwrite = TRUE)
usethis::use_data(selfComp.names.nf3, overwrite = TRUE)
usethis::use_data(selfContempt.names.nf3, overwrite = TRUE)
usethis::use_data(selfCrit.names, overwrite = TRUE)
usethis::use_data(single.items.names, overwrite = TRUE)
usethis::use_data(single.items.names.nf3, overwrite = TRUE)
usethis::use_data(socAvoid.names.nf3, overwrite = TRUE)
usethis::use_data(socialSafety.names, overwrite = TRUE)
usethis::use_data(socSup.names.nf3, overwrite = TRUE)
usethis::use_data(somAnx.names, overwrite = TRUE)
usethis::use_data(subRecov.names, overwrite = TRUE)
usethis::use_data(subUse.names, overwrite = TRUE)
usethis::use_data(subUse.names.nf3, overwrite = TRUE)
usethis::use_data(suicide.names, overwrite = TRUE)
usethis::use_data(suicide.names.nf3, overwrite = TRUE)
usethis::use_data(trauma.names, overwrite = TRUE)
usethis::use_data(worry.names, overwrite = TRUE)
usethis::use_data(worry.names.nf3, overwrite = TRUE)

