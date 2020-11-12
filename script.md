if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
if(!require(ggpubr)){
    install.packages("ggpubr")
    library(ggpubr)
}
if(!require(car)){
    install.packages("car")
    library(car)
}
if(!require(sjstats)){
    install.packages("sjstats")
    library(sjstats)
}
if(!require(emmeans)){
    install.packages("emmeans")
    library(emmans)
}
if(!require(broom)){
    install.packages("broom")
    library(broom)
}
if(!require(readr)){
    install.packages("readr")
    library(readr)
}
if(!require(ltm)){
    install.packages("ltm")
    library(ltm)
}


#Neuromyths
neuromyths <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/lapn/master/theoretical.csv",
col_types = cols(AssessmentPeriod = col_factor(levels = c("BEFORE", "AFTER")), Group = col_factor(levels = c("CTRL", "EXP")))) #load data

ngmyths <- neuromyths[,-c(1:2)] #remove group labels
grnmCB <- ngmyths[c(1:130),] #separate subgroups
grnmCA <- ngmyths[c(131:227),] #separate subgroups
grnmEB <- ngmyths[c(132:369),] #separate subgroups
grnmEA <- ngmyths[c(370:473),] #separate subgroups

CB_raschmodel <- RM(grnmCB) #Rasch Model using conditional maximum likelihood (CML) estimation of the item parameters
tmp1 <- person.parameter(CB_raschmodel) #Person parameters, with ML estimation
pp_m1 <- coef(tmp1) #Person parameters coefficients, with ML estimation
plotjointICC(CB_raschmodel, cex = .6) #Joint ICC plots for all items on subgroup

CA_raschmodel <- RM(grnmCA) #Rasch Model using conditional maximum likelihood (CML) estimation of the item parameters
tmp2 <- person.parameter(CA_raschmodel) #Person parameters, with ML estimation
pp_m2 <- coef(tmp2)#Person parameters coefficients, with ML estimation
plotjointICC(CA_raschmodel, cex = .6) #Joint ICC plots for all items on subgroup

EB_raschmodel <- RM(grnmEB) #Rasch Model using conditional maximum likelihood (CML) estimation of the item parameters
tmp3 <- person.parameter(EB_raschmodel) #Person parameters, with ML estimation
pp_m3 <- coef(tmp3)#Person parameters coefficients, with ML estimation
plotjointICC(EB_raschmodel, cex = .6) #Joint ICC plots for all items on subgroup

EA_raschmodel <- RM(grnmEA) #Rasch Model using conditional maximum likelihood (CML) estimation of the item parameters
tmp4 <- person.parameter(EA_raschmodel) #Person parameters, with ML estimation
pp_m4 <- coef(tmp4)#Person parameters coefficients, with ML estimation
plotjointICC(EA_raschmodel, cex = .6) #Joint ICC plots for all items on subgroup

df1 <- merge(x = tmp1$theta.table[1], y = tmp2$theta.table[1], all = TRUE)
df2 <- merge(x = tmp3$theta.table[1], y = tmp4$theta.table[1], all = TRUE)
rasch_scores <- merge(x = df1, y = df2, all = TRUE) #merge individual tables

#Part A
LAPN_attitudes_partA <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/lapn/master/LAPN-attitudes-partA.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "EXP")), AssessmentPeriod = col_factor(levels = c("BEFORE", "AFTER"))))

Gr <- paste(LAPN_attitudes_partA$Group, LAPN_attitudes_partA$AssessmentPeriod, sep = " ") #Merge group and assessment period to generate Likert-type plots
likpartA <- LAPN_attitudes_partA[,-c(1:2)]
likpartA$Gr <- Gr
likpartA[sapply(likpartA, is.character)] <- lapply(likpartA[sapply(likpartA, is.character)],
                                               as.factor)
newfac <- function(x, oldval, newval, ordered=TRUE) {
    factor(newval[match(x, oldval)], ordered=TRUE)
}
likpartA[, c(1:15)] <- sapply(likpartA[,1:15], newfac, oldval = c("1", "2", "3", "4", "5"), newval = c("Totally disagree", "Partially disagree", "Neither agree nor disagree", "Partially agree", "Totally agree"))

surveymv::surveyPlot(data = likpartA, vars = vars("The scientist's work sounds like fun", "I enjoy science classes", "I'm good at science", "I want to be a scientist in the future", "I'll probably go to do some college science course", "The science classes I have in school will help me in the future", "I generally understand what is taught in science classes", "Learning new things will never change your intelligence", "I can get smarter if I study hard", "Only very good students will go to any science course in college", "I'm good at science classes because I'm smart", "I don't do well in science classes because I'm not smart", "I don’t want to be a scientist in the future", "For me, it's more important to learn new things in class than to get good grades in exams.", "Understanding what is taught is more important than memorizing what is taught"), group = Gr,type = "stacked", freq = "perc") #Draw stacked bar plots

ngLAPN <- LAPN_attitudes_partA[,-c(1:2)]
ngLAPN$`Learning new things will never change your intelligence` <- 6-ngLAPN$`Learning new things will never change your intelligence` #reverse scoring
ngLAPN$`Only very good students will go to any science course in college` <- 6-ngLAPN$`Only very good students will go to any science course in college` #reverse scoring
ngLAPN$`I don't do well in science classes because I'm not smart` <- 6-ngLAPN$`I don't do well in science classes because I'm not smart` #reverse scoring
ngLAPN$`I don’t want to be a scientist in the future` <- 6-ngLAPN$`I don’t want to be a scientist in the future` #reverse scoring

ngLAPN[sapply(ngLAPN, is.character)] <- lapply(ngLAPN[sapply(ngLAPN, is.character)],
  as.factor)
newfac <- function(x, oldval, newval, ordered=TRUE) {
  factor(newval[match(x, oldval)], ordered=TRUE)
}
ngLAPN[, c(1:15)] <- sapply(ngLAPN[,1:15], newfac, oldval = c("1", "2", "3", "4", "5"), newval = c("Totally disagree", "Partially disagree", "Neither agree nor disagree", "Partially agree", "Totally agree"))

ngLAPN[sapply(ngLAPN, is.character)] <- lapply(ngLAPN[sapply(ngLAPN, is.character)],
  as.factor)

grCB <- ngLAPN[c(1:130),]
grCA <- ngLAPN[c(131:227),]
grEB <- ngLAPN[c(132:369),]
grEA <- ngLAPN[c(370:473),]

GrmCB = grm(grCB)
GrmCA = grm(grCA)
GrmEB = grm(grEB)
GrmEA = grm(grEA)

TTCB <- factor.scores(GrmCB, resp.patterns = data.frame(lapply(grCB, as.numeric)))[["score.dat"]]
TTCA <- factor.scores(GrmCA, resp.patterns = data.frame(lapply(grCA, as.numeric)))[["score.dat"]]
TTEB <- factor.scores(GrmEB, resp.patterns = data.frame(lapply(grEB, as.numeric)))[["score.dat"]]
TTEA <- factor.scores(GrmEA, resp.patterns = data.frame(lapply(grEA, as.numeric)))[["score.dat"]]

TTCB[["Gr"]] <- c("CTRL")
TTCB[["AP"]] <- c("BEFORE")
TTCA[["Gr"]] <- c("CTRL")
TTCA[["AP"]] <- c("AFTER")
TTEB[["Gr"]] <- c("EXP")
TTEB[["AP"]] <- c("BEFORE")
TTEA[["Gr"]] <- c("EXP")
TTEA[["AP"]] <- c("AFTER")
df1 <- merge(x = TTCB, y = TTCA, all = TRUE)
df2 <- merge(x = TTEB, y = TTEA, all = TRUE)
LAPN_attitudes_scores_partA <- merge(x = df1, y = df2, all = TRUE)
LAPN_attitudes_scores_partA$PP <- rasch_scores$`Person Parameter`
LAPN_attitudes_scores_partA$AP <- factor(LAPN_attitudes_scores_partA$AP, levels = c("BEFORE", "AFTER"))


attitude_partA <- aov(data = LAPN_attitudes_scores_partA, z1 ~ Gr + AP + Gr*AP) #ANOVA
summary(attitude_partBA)
TukeyHSD(attitude_partA)
car::Anova(attitude_partA, type = 2)
sjstats::anova_stats(car::Anova(attitude_partA, type = 2))

fig1A <- ggplot(LAPN_attitudes_scores_partA, aes(x = Gr, y = z1, colour = AP)) + geom_boxplot(outlier.shape = NA) + geom_point(aes(color = AP), position = position_jitterdodge()) + ylim(-3, 3) + labs(colour = "Assessment period", x = "Group", y = "Factor score (z)", title = "Science Attitude Score, Part A", subtitle = "Views on self", tag = "A") #plot

#Part B
LAPN_attitudes_partB <- read_csv("https://raw.githubusercontent.com/lanec-unifesspa/lapn/master/LAPN-attitudes-partB.csv", col_types = cols(Group = col_factor(levels = c("CTRL", "EXP")), AssessmentPeriod = col_factor(levels = c("BEFORE", "AFTER"))))

Gr <- paste(LAPN_attitudes_partB$Group, LAPN_attitudes_partB$AssessmentPeriod, sep = " ") #Merge group and assessment period to generate Likert-type plots
likpartB <- LAPN_attitudes_partB[,-c(1:2)]
likpartB$Gr <- Gr
likpartB[sapply(likpartB, is.character)] <- lapply(likpartB[sapply(likpartB, is.character)],
                                               as.factor)
newfac <- function(x, oldval, newval, ordered=TRUE) {
    factor(newval[match(x, oldval)], ordered=TRUE)
}
likpartB[, c(1:8)] <- sapply(likpartB[,1:8], newfac, oldval = c("1", "2", "3", "4", "5"), newval = c("Totally disagree", "Partially disagree", "Neither agree nor disagree", "Partially agree", "Totally agree"))

surveymv::surveyPlot(data = likpartB, vars = vars("Scientists often need to educate the general public on important issues", "Scientists usually work in teams", "Scientists usually engage well with the general public", "Scientists get high salaries", "Scientists are usually financially poor", "It's hard to get a job as a scientist"), group = Gr,type = "stacked", freq = "perc") #Draw stacked bar plots

ngLAPN2 <- LAPN_attitudes_partB[,-c(1:2)] #remove groups
ngLAPN2$`Scientists are usually financially poor` <- 6-ngLAPN2$`Scientists are usually financially poor` #reverse scoring
ngLAPN2$`It's hard to get a job as a scientist` <- 6-ngLAPN2$`It's hard to get a job as a scientist` #reverse scoring
ngLAPN2$`Scientists are antisocial people` <- 6-ngLAPN2$`Scientists are antisocial people` #reverse scoring
ngLAPN2$`Scientists are tedious people` <- 6-ngLAPN2$`Scientists are tedious people` #reverse scoring

ngLAPN2[sapply(ngLAPN2, is.character)] <- lapply(ngLAPN2[sapply(ngLAPN2, is.character)],
  as.factor)
newfac <- function(x, oldval, newval, ordered=TRUE) {
  factor(newval[match(x, oldval)], ordered=TRUE)
}
ngLAPN2[, c(1:8)] <- sapply(ngLAPN2[,1:8], newfac, oldval = c("1", "2", "3", "4", "5"), newval = c("Totally disagree", "Partially disagree", "Neither agree nor disagree", "Partially agree", "Totally agree"))

ngLAPN2[sapply(ngLAPN2, is.character)] <- lapply(ngLAPN2[sapply(ngLAPN2, is.character)],
  as.factor)

grCB2 <- ngLAPN2[c(1:130),] #separate dataset intro groups to calculate z scores
grCA2 <- ngLAPN2[c(131:227),]
grEB2 <- ngLAPN2[c(132:369),]
grEA2 <- ngLAPN2[c(370:473),]

GrmCB2 = grm(grCB2) #Graded response model
GrmCA2 = grm(grCA2)
GrmEB2 = grm(grEB2)
GrmEA2 = grm(grEA2)

TTCB2 <- factor.scores(GrmCB2, resp.patterns = data.frame(lapply(grCB2, as.numeric)))[["score.dat"]] #calculate z scores
TTCA2 <- factor.scores(GrmCA2, resp.patterns = data.frame(lapply(grCA2, as.numeric)))[["score.dat"]]
TTEB2 <- factor.scores(GrmEB2, resp.patterns = data.frame(lapply(grEB2, as.numeric)))[["score.dat"]]
TTEA2 <- factor.scores(GrmEA2, resp.patterns = data.frame(lapply(grEA2, as.numeric)))[["score.dat"]]

TTCB2[["Gr"]] <- c("CTRL")
TTCB2[["AP"]] <- c("BEFORE")
TTCA2[["Gr"]] <- c("CTRL")
TTCA2[["AP"]] <- c("AFTER")
TTEB2[["Gr"]] <- c("EXP")
TTEB2[["AP"]] <- c("BEFORE")
TTEA2[["Gr"]] <- c("EXP")
TTEA2[["AP"]] <- c("AFTER")
df1 <- merge(x = TTCB2, y = TTCA2, all = TRUE)
df2 <- merge(x = TTEB2, y = TTEA2, all = TRUE)
LAPN_attitudes_scores_partB <- merge(x = df1, y = df2, all = TRUE) #Re-merge dataset
LAPN_attitudes_scores_partB$PP <- rasch_scores$`Person Parameter`
LAPN_attitudes_scores_partB$AP <- factor(LAPN_attitudes_scores_partB$AP, levels = c("BEFORE", "AFTER"))


attitude_partB <- aov(data = LAPN_attitudes_scores_partB, z1 ~ Gr + AP + Gr*AP) #ANOVA
summary(attitude_partB)
TukeyHSD(attitude_partB)
car::Anova(attitude_partB, type = 2)
sjstats::anova_stats(car::Anova(attitude_partB, type = 2))

fig1B <- ggplot(LAPN_attitudes_scores_partB, aes(x = Gr, y = z1, colour = AP)) + geom_boxplot(outlier.shape = NA) + geom_point(aes(color = AP), position = position_jitterdodge()) + ylim(-3, 3) + labs(colour = "Assessment period", x = "Group", y = "Factor score (z)", title = "Science Attitude Score, Part B", subtitle = "Views on scientists", tag = "B", caption = "Factor scores calculated with a graded response model") #plot



car::Anova(attitude_partA, type = 2)
sjstats::anova_stats(car::Anova(attitude_partA, type = 2))
aovSAS_partA <- broom::tidy(emmeans::pmmeans(attitude_partA, "AP", by = "Gr"))
ggplot(data = aovSAS, aes(x = AssessmentPeriod, y = estimate, group = Group, color = Group, fill = Group)) + geom_point(shape = 18, size = 4) + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + geom_line(size = 1) + ylim(0, 100) + ggrepel::geom_label_repel(aes(label = round(estimate, 2)), nudge_x = -.35, color = "black") + geom_jitter(data = escores, aes(y = ScienceAttitudeScore, x = AssessmentPeriod, group = Group, color = Group), width = 0.2) + xlab("Assessment period") + ylab("Science Attitude Score") + ggtitle(expression(eta ^2 ~"(Group) = 0.021," ~ eta ^2 ~"(Assessment period) = 0.065," ~ eta ^2 ~"(int.) = 0.11")) + theme(plot.title = element_text(size = 10))

#Analyze neuromyths data
nocov <- aov(data = LAPN_attitudes_partA, PP ~ Gr * AP) #Without co-variate
car::Anova(nocov, type = 2) #Type II ANOVA
sjstats::anova_stats(car::Anova(nocov, type = 2)) #Effect sizes and power
broom::glance(nocov) #Fit statistics

broom::glance(lm(PP ~ z1, LAPN_attitudes_scores_partA)) #Look at relationship between attitude scores and correct responses, for Part A of questionnaire
ggplot(LAPN_attitudes_scores_partA, aes(x = z1, y = PP)) + geom_point(alpha = 1/10) + geom_smooth(method = "loess", color = "red") + geom_smooth(method = "lm", color = "blue") #Plot relationship between attitude scores and correct responses, for Part A of questionnaire
broom::glance(lm(PP ~ z1, LAPN_attitudes_scores_partB)) #Look at relationship between attitude scores and correct responses, for Part A of questionnaire
ggplot(LAPN_attitudes_scores_partB, aes(x = z1, y = PP)) + geom_point(alpha = 1/10) + geom_smooth(method = "loess", color = "red") + geom_smooth(method = "lm", color = "blue") #Plot relationship between attitude scores and correct responses, for Part B of questionnaire


cov_ptA <- aov(data = LAPN_attitudes_scores_partA, PP ~ Gr * AP + z1)
car::Anova(cov_ptA, type = 2) #Type II ANOVA
broom::glance(cov_ptA) #Fit statistics
sjstats::anova_stats(car::Anova(cov_ptA, type = 2)) #Effect sizes and power

cov_ptB <- aov(data = LAPN_attitudes_scores_partB, PP ~ Gr * AP + z1)
car::Anova(cov_ptB, type = 2) #Type II ANOVA
broom::glance(cov_ptB) #Fit statistics
sjstats::anova_stats(car::Anova(cov_ptB, type = 2)) #Effect sizes and power

emmeans::pmmeans(nocov, "AP", by = "Gr") #Calculate unadjusted marginal means
emmeans::emmip(nocov, AP ~ Gr, CIs = TRUE) #Plot unadjusted marginal means
pairs(emmeans::pmmeans(nocov, "AP", by = "Gr"), adjust = "scheffe") #Contrasts for unadjusted marginal means
emmeans::pmmeans(cov_ptA, "z1") #Marginal means corrected by covariate, for Part A of the questionnaire
emmeans::pmmeans(cov_ptB, "z1") #Marginal means corrected by covariate, for Part B of the questionnaire
emmeans::pmmeans(cov_ptA, "AP", by = "Gr")
emmeans::pmmeans(cov_ptB, "AP", by = "Gr")
emmeans::emmip(cov_ptA, AP ~ Gr, CIs = TRUE) #Plot marginal means corrected by covariate, for Part A of the questionnaire
emmeans::emmip(cov_ptB, AP ~ Gr, CIs = TRUE) #Plot marginal means corrected by covariate, for Part B of the questionnaire
pairs(emmeans::pmmeans(cov_ptA, "AP", by = "Gr"), adjust = "scheffe") #Contrasts for marginal means corrected by covariate, for Part A of the questionnaire
pairs(emmeans::pmmeans(cov_ptB, "AP", by = "Gr"), adjust = "scheffe") #Contrasts for marginal means corrected by covariate, for Part B of the questionnaire
withCOV_ptA <- broom::tidy(summary(emmeans::pmmeans(cov_ptA, "AP", by = "Gr")))
withCOV_ptB <- broom::tidy(summary(emmeans::pmmeans(cov_ptB, "AP", by = "Gr")))
noCOV <- broom::tidy(emmeans::pmmeans(nocov, "AP", by = "Gr"))
ggplot(data = withCOV_ptA, aes(x = AP, y = estimate, group = Gr, color = Gr)) + geom_point(shape = 18, size = 4) + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + geom_line(size = 1)
ggplot(data = withCOV_ptB, aes(x = AP, y = estimate, group = Gr, color = Gr)) + geom_point(shape = 18, size = 4) + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + geom_line(size = 1)