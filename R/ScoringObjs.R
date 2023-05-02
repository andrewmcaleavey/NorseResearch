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

# item_norm_table <- NorseResearch::HF_research_data_2021_fscores

