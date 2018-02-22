-------- # Load files and pre processing -----
# load files
samples_d = read.table("eyetracking_2018_samples.txt", sep= "\t", header = T)
fixations_d = read.table("eyetracking_2018_fixations.txt", sep= "\t", header = T)
saccades_d = read.table("eyetracking_2018_saccades.txt", sep= "\t", header = T)

# load V1 files
fixation_V1 = read.csv("FixationsV1.csv")
saccades_V1 = read.csv("SaccadesV1.csv")
samples_V1 = read.csv("SamplesV1.csv")

# load V2 files
fix_V2 = read.csv("FixationsV2.csv")
samples_V2 = read.csv("SamplesV2.csv")
samples_v3 = read.csv("SamplesV3.csv")

# load samples V3 and downsample
sub_samp = subset(samples_v3, Task == "SocialEngagement")

# how to downsample

# make condition
fixation_V1$Condition = 0
fixation_V1$Condition[fixation_V1$SearchOrder == 1 & fixation_V1$Trial < 6] = "Star"
fixation_V1$Condition[fixation_V1$SearchOrder == 1 & fixation_V1$Trial > 5] = "Count"

fixation_V1$Condition[fixation_V1$SearchOrder == 2 & fixation_V1$Trial < 6] = "Count"
fixation_V1$Condition[fixation_V1$SearchOrder == 2 & fixation_V1$Trial > 5] = "Star"

# load log files
log_1 = read.csv("PupilsLogs/logfile_1_2_f.csv")
log_2 = read.csv("PupilsLogs/logfile_2_1_f.csv")
log_3 = read.csv("PupilsLogs/logfile_3_2_f.csv")
log_4 = read.csv("PupilsLogs/logfile_4_1_F.csv")
log_5 = read.csv("PupilsLogs/logfile_5_2_m.csv")
log_6 = read.csv("PupilsLogs/logfile_6_1_m.csv")

#setwd("~/Eye Tracking/2018 - Eye tracking/PupilsLogs")
#filelist = list.files(path = ".", pattern = "logfile")
#log = lapply(filelist, read.csv)

# merge log files and setnames
log = rbind(log_1, log_2, log_3, log_4, log_5, log_6)

log = set_names(log[1:3], c("Trial", "ParticipantID", "video"))

log$Trial = log$Trial + 1

# merge data with log
fix_d = merge(fixation_V1, log, by = c("Trial", "ParticipantID"), all = T)

# extract variables from the colun "video" in the log files
# gender
fix_d$Video_gender = substring(fix_d$video, 1, 1)

# direction
fix_d$Direction = substring(fix_d$video,9,11)

# ostensiveness
fix_d$Ostensiveness = substring(fix_d$video, 13,14)

fix_d$Ostensiveness[fix_d$Ostensiveness == "+o"] = "eyecontact"
fix_d$Ostensiveness[fix_d$Ostensiveness == "-o"] = "noeyecontact"

# complete this for all V1 files so that they look like the V2 files in the folder