
# CALL LIBRARIES ----------------------------------------------------------

library(foreign)
library(plotrix)
library(MASS)


# READ IN ANES FILE --------------------------------------------

anes.full <- read.csv("Women in 2016 Election Final.csv") #Read in cleaned data
head(anes.full)
nrow(anes.full)



# ORGANIZE DATA ------------------------------------------------------

standardize <- function(x,...){(x-min(x,...))/(max(x,...)-min(x,...))} #Write standarization function

sexism <- anes.full$sexism_hostile #Create vectors to be manipulated
antifeminism <- anes.full$femscale

anes.full$s.sexism_hostile <- standardize(anes.full$sexism, na.rm = TRUE) #Standardize the sexism scale
anes.full$s.femscale <- standardize(antifeminism, na.rm = TRUE)


anes <- subset(anes.full, race == "White, nonHispanic") #Created demographic subsets
summary(anes$race)

women <- subset(anes, female == 1)
men <- subset(anes, female == 0)
republican <- subset(anes, pid7_r >= 5)
democrat <- subset(anes, pid7_r <=3)


d.women <- subset(women, pid7_r <= 3)
r.women <- subset(women, pid7_r >= 5)
dw.novote <- subset(d.women, vote16 == 0)
rw.novote <- subset(r.women, vote16 == 0)
ptrump.women <- subset(women, whoprim_r == 1 & pid7_r >=5)
gtrump.women <- subset(women, who16_r == 1 & pid7_r >=5)
strump.women <- subset(women, switch_trump == 1)
sabstain.women <- subset(women, abstainer_trump == 1)



d.men <- subset(men, pid7_r <= 3)
r.men <- subset(men, pid7_r >= 5)
dm.novote <- subset(d.men, vote16 == 0)
rm.novote <- subset(r.men, vote16 == 0)
ptrump.men <- subset(men, whoprim_trump == 1 & pid7_r >=5)
gtrump.men <- subset(men, who16_r == 1 & pid7_r >=5)
strump.men <- subset(men, switch_trump == 1& pid7_r >=5)
sabstain.men <- subset(men, abstainer_trump == 1& pid7_r >=5)



# COMPARE SEXISM BY PARTISANSHIP AND GENDER -------------------------------

sexism.avg <- c(mean(r.women$s.sexism_hostile, na.rm = TRUE), mean(r.men$s.sexism_hostile, na.rm = TRUE), mean(d.women$s.sexism_hostile, na.rm = TRUE), mean(d.men$s.sexism_hostile, na.rm = TRUE))
sexism.avg

std.error <- function(x) {sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

sexism.se <- c(std.error(r.women$s.sexism_hostile), std.error(r.men$s.sexism_hostile), std.error(d.women$s.sexism_hostile), std.error(d.men$s.sexism_hostile))
sexism.se

sexism.ci <- sexism.se*1.96
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

par(family = "serif") #Set text
attach(mtcars)

bp <- barplot(sexism.avg, space = c(0, .1, 1, .1), ylim = c(0,.6), names.arg = c("Republican\nWomen", "Republican\nMen", "Democratic\nWomen","Democratic\nMen"), main = "Hostile Sexism by Gender and Partisanship", cex.main = .97)
error.bar(bp, sexism.avg, sexism.ci)
abline(h = 0, lwd = 2)



# HOSTILE SEXISM BY VOTE CHOICE --------

frvoters <- subset(anes, female == 1 & voteprim == 1 & pid2_r == 1)

table(frvoters$whoprim)

far <- subset(frvoters, whoprim == "Another Republican")
fhc <- subset(frvoters, whoprim == "Hillary Clinton")
fmr <- subset(frvoters, whoprim == "Marco Rubio")
fse <- subset(frvoters, whoprim == "Someone else who is not a Republican or Democrat")
fod <- subset(frvoters, whoprim == "Another Democrat")
fbs <- subset(frvoters, whoprim == "Bernie Sanders")
fdt <- subset(frvoters, whoprim == "Donald Trump")
fjk <- subset(frvoters, whoprim == "John Kasich")
ftc <- subset(frvoters, whoprim == "Ted Cruz")


mrvoters <- subset(anes, female == 0 & voteprim == 1 & pid2_r == 1)

table(mrvoters$whoprim)

mar <- subset(mrvoters, whoprim == "Another Republican")
mhc <- subset(mrvoters, whoprim == "Hillary Clinton")
mmr <- subset(mrvoters, whoprim == "Marco Rubio")
mse <- subset(mrvoters, whoprim == "Someone else who is not a Republican or Democrat")
mod <- subset(mrvoters, whoprim == "Another Democrat")
mbs <- subset(mrvoters, whoprim == "Bernie Sanders")
mdt <- subset(mrvoters, whoprim == "Donald Trump")
mjk <- subset(mrvoters, whoprim == "John Kasich")
mtc <- subset(mrvoters, whoprim == "Ted Cruz")


sexism.avg <- c(mean(fdt$s.sexism_hostile, na.rm = TRUE), mean(mdt$s.sexism_hostile, na.rm = TRUE), mean(ftc$s.sexism_hostile, na.rm = TRUE), mean(mtc$s.sexism_hostile, na.rm = TRUE), mean(fjk$s.sexism_hostile, na.rm = TRUE), mean(mjk$s.sexism_hostile, na.rm = TRUE), mean(fmr$s.sexism_hostile, na.rm = TRUE), mean(mmr$s.sexism_hostile, na.rm = TRUE), mean(fbs$s.sexism_hostile, na.rm = TRUE), mean(mbs$s.sexism_hostile, na.rm = TRUE), mean(fhc$s.sexism_hostile, na.rm = TRUE), mean(mhc$s.sexism_hostile, na.rm = TRUE))
sexism.avg

std.error <- function(x) {sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

sexism.se <- c(std.error(fdt$s.sexism_hostile), std.error(mdt$s.sexism_hostile), std.error(ftc$s.sexism_hostile), std.error(mtc$s.sexism_hostile), std.error(fjk$s.sexism_hostile), std.error(mjk$s.sexism_hostile), std.error(fmr$s.sexism_hostile), std.error(mmr$s.sexism_hostile), std.error(fbs$s.sexism_hostile), std.error(mbs$s.sexism_hostile), std.error(fhc$s.sexism_hostile), std.error(mhc$s.sexism_hostile))
sexism.se

sexism.ci <- sexism.se*1.96

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

r.prim <- subset(anes, voteprim == 1, pid2_r == 1)

sexism.n <- as.character(c(nrow(fdt)/nrow(frvoters), nrow(mdt)/nrow(mrvoters), nrow(ftc)/nrow(frvoters), nrow(mtc)/nrow(mrvoters), nrow(fjk)/nrow(frvoters), nrow(mjk)/nrow(mrvoters), nrow(fmr)/nrow(frvoters), nrow(mmr)/nrow(mrvoters), nrow(fbs)/nrow(frvoters), nrow(mbs)/nrow(mrvoters), nrow(fhc)/nrow(frvoters), nrow(mhc)/nrow(mrvoters)))

par(family = "serif") #Set text

bp <- barplot(sexism.avg, col = c("grey", "white"), ylim = c(0,.8), space = c(0, .1, .4, .1, .4, .1, .4, .1, .4, .1, .4, .1), main = "Hostile Sexism by Primary Vote Choice Among Republicans", cex.main = .97)
error.bar(bp, sexism.avg, sexism.ci)
abline(h = 0, lwd = 2)
mtext(at = c(1.05, 3.55, 6.05, 8.55, 11.05, 13.55), side = 1, line = .15, text = c("Trump", "Cruz", "Kasich", "Rubio", "Sanders", "Clinton"), cex = .95)
legend("topleft", c("Women", "Men"), , pch = c(22, 22), col = c("black", "black"), pt.bg = c("grey", "white"), bty = "n", cex = .9)



# PREDICTORS OF PRIMARY VOTE CHOICE AMONG REPUBLICANS --------


frvoters <- subset(anes, female == 1 & whoprim_r == 1 & pid2_r == 1)

m1 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend, family=binomial(link="logit"), data = frvoters)
summary(m1) #Present

m2 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend + sexism_hostile, family=binomial(link="logit"), data = frvoters)
summary(m2) #Present
newdata <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), sexism_hostile = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
womenprim.sex <- predict(m2, newdata, type = "response", se.fit = TRUE)
womenprim.sex.ciu <- womenprim.sex$fit*100 + womenprim.sex$se.fit*100*1.96
womenprim.sex.cil <- womenprim.sex$fit*100 - womenprim.sex$se.fit*100*1.96
womenprim.sex <- womenprim.sex$fit*100


m3 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend + femscale, family=binomial(link="logit"), data = frvoters)
summary(m3) #Present
newdata <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), femscale = c(1, 2, 3, 4, 5))
womenprim.fem <- predict(m3, newdata, type = "response", se.fit = TRUE)
womenprim.fem.ciu <- womenprim.fem$fit*100 + womenprim.fem$se.fit*100*1.96
womenprim.fem.cil <- womenprim.fem$fit*100 - womenprim.fem$se.fit*100*1.96
womenprim.fem <- womenprim.fem$fit*100


fmvoters <- subset(anes, female == 0 & whoprim_r == 1 & pid2_r == 1)

m1 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend, family=binomial(link="logit"), data = fmvoters)
summary(m1) #Present

m2 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend + sexism_hostile, family=binomial(link="logit"), data = fmvoters)
summary(m2) #Present

m3 <- glm(whoprim_trump ~ pid7_r + age + income + education + married + attend + femscale, family=binomial(link="logit"), data = fmvoters)
summary(m3) #Present



attach(mtcars)
par(mfrow=c(2,1),
    mar = c(3, 4, 4, 1))
par(family = "serif") #Set text


plot(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), womenprim.sex, type = "l", ylim = c(0, 100), main = "Probability of Voting for Trump in the Primary\nby Gender Attitudes Among Republican Women", xlab = "Hostile Sexism", ylab = "Probability of Voting for Trump", cex.main = .9)
points(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), womenprim.sex, pch = 16)
lines(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), womenprim.sex.cil, lty = 2)
lines(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), womenprim.sex.ciu, lty = 2)



plot(c(1, 2, 3, 4, 5), womenprim.fem, type = "l", ylim = c(0, 100), xlab = "Feminism", ylab = "Probability of Voting for Trump")
points(c(1, 2, 3, 4, 5), womenprim.fem, pch = 16)
lines(c(1, 2, 3, 4, 5), womenprim.fem.cil, lty = 2)
lines(c(1, 2, 3, 4, 5), womenprim.fem.ciu, lty = 2)



# TRUMP'S LOST PRIMARY VOTES BY GENDER ATTITUDES --------


frvoters <- subset(anes, female == 1 & whoprim_r == 1 & pid2_r == 1)

womensexism <- c(table(frvoters$sexism_hostile))
womensexism.perc <- c(prop.table(table(frvoters$sexism_hostile)))*100
womenpredict.sex <- c(womenprim.sex)/100

trumpvoters.sexism <- data.frame(cbind(womensexism, womensexism.perc, womenpredict.sex))
trumpvoters.sexism
trumpvoters.sexism$lostvotes <- womensexism.perc * (1 - womenpredict.sex)
trumpvoters.sexism$wonvotes <- womensexism.perc * womenpredict.sex
trumpvoters.sexism

counts <- rbind(trumpvoters.sexism$lostvotes, trumpvoters.sexism$wonvotes)
rownames(counts) <- c("Votes Lost", "Votes Won")

par(mar=c(5,4.1,4,2.1))
par(family = "serif") #Set text

bp <- barplot(as.matrix(counts), ylab = "Percentage", names.arg = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), xlab = "Level of Hostile Sexism", col = c("gray37", "lightgray"), space = c(0, .8, .2, .2, .2, .2, .2, .2, .2, .2, .2, .2), ylim = c(0, 25), main = "Hostile Sexism", cex.main = .9)
text(bp, trumpvoters.sexism$lostvotes + trumpvoters.sexism$wonvotes + .55, c(" 4.1%", " 3.2%", " 4.4%", " 3.9%", " 5.9%", " 5.5%"," 11.6%", " 5.9%", " 5.2%", " 2.5%", " 1.9%", " 1.1%"), cex = .6)
text(bp, trumpvoters.sexism$lostvotes + .55, c(" 4.3%", " 3.1%", " 4.1%", " 3.5%", " 5.0%", " 4.4%", " 8.9%", " 4.3%", " 3.6%", " 1.7%", " 1.2%", " 0.7%"), cex = .6)
abline(h = 0, lwd = 2)
lines(c(1.4, 1.4), c(0,20), lwd = 1.5, lty =3)
legend("topleft", c("Votes Won", "Votes Lost", "Cutoff for Electoral Advantage"), pch = c(22, 22, 26), lty = c(0, 0, 3), lwd = c(0, 0, 1),  col = c("black", "black"), pt.bg = c("lightgray", "gray37"), bty = "n", cex = .9)







frvoters <- subset(anes, female == 1 & whoprim_r == 1 & pid2_r == 1)

womenfeminism <- c(table(frvoters$femscale))
womenfeminism.perc <- c(prop.table(table(frvoters$femscale)))*100
womenpredict.fem <- c(womenprim.fem)/100

trumpvoters.feminism <- data.frame(cbind(womenfeminism, womenfeminism.perc, womenpredict.fem))
womenfeminism
womenfeminism$lostvotes <- womenfeminism.perc * (1 - womenpredict.fem)
womenfeminism$wonvotes <- womenfeminism.perc * womenpredict.fem
womenfeminism

counts <- rbind(womenfeminism$lostvotes, womenfeminism$wonvotes)
rownames(counts) <- c("Votes Lost", "Votes Won")

par(mar=c(5,4.1,4,2.1))
par(family = "serif") #Set text

bp <- barplot(as.matrix(counts), ylab = "Percentage", names.arg = c(1, 2, 3, 4, 5), xlab = "Level of Feminism", col = c("gray37", "lightgray"), space = c(0, .2, .2, .8, .2), ylim = c(0, 60), main = "Feminism", cex.main = .9)
text(bp, womenfeminism$lostvotes + womenfeminism$wonvotes + .55, c(" 20.2%", " 13.6%", " 7.9%", " 2.0%", " 1.2%"), cex = .6)
text(bp, womenfeminism$lostvotes + .45, c(" 28.2%", " 16.2%", " 8.0%", " 1.8%", " 0.9%"), cex = .6)
abline(h = 0, lwd = 2)
lines(c(3.8, 3.8), c(0,55), lwd = 1.5, lty =3)
legend("topleft", c("Votes Won", "Votes Lost", "Cutoff for Electoral Advantage"), pch = c(22, 22, 26), lty = c(0, 0, 3), lwd = c(0, 0, 1),  col = c("black", "black"), pt.bg = c("lightgray", "gray37"), bty = "n", cex = .9)



# PREDICTORS OF GENERAL TRUMP VOTE AMONG REPUBLICAN WOMEN -------------------------

frvoters <- subset(anes, vote16 == 1 & pid2_r == 1 & female == 1)

m1 <- glm(who16_trump ~ pid7_r + age + income + education + married + attend, family = binomial(link="logit"), data = frvoters)
summary(m1) #Present


m2 <- glm(who16_trump ~ pid7_r + age+ income + education + married + attend + sexism_hostile.rc, family=binomial(link="logit"), data = frvoters)
summary(m2) #Present
newdata1 <- data.frame(female = 1, pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), sexism_hostile.rc = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
womenpredict1 <- predict(m2, newdata1, type = "response", se.fit = TRUE)
womenpredict1


m3 <- glm(who16_trump ~ pid7_r + age + income + education + married + attend + femscale, family=binomial(link="logit"), data = frvoters)
summary(m3) #Present
newdata1 <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), femscale = c(1, 2, 3, 4, 5))
womenpredict2 <- predict(m3, newdata1, type = "response", se.fit = TRUE)
womenpredict2


m4 <- glm(who16_trump ~ pid7_r + age + income + education + married + attend + mattervideo, family=binomial(link="logit"), data = frvoters)
summary(m4) #Present
newdata1 <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), mattervideo = c(1, 2, 3, 4, 5))
womenpredict3 <- predict(m4, newdata1, type = "response", se.fit = TRUE)
womenpredict3


m5 <- glm(who16_trump ~ pid7_r + age + income + education + married + attend + repwomen.rc, family=binomial(link="logit"), data = frvoters)
summary(m5) #Present
newdata1 <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), repwomen.rc = c(1, 2, 3, 4, 5, 6, 7))
womenpredict4 <- predict(m5, newdata1, type = "response", se.fit = TRUE)
womenpredict4




attach(mtcars)
par(mfrow=c(4,1),
    mar = c(3, 4, 4, 1))
par(family = "serif") #Set text

plot(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), xaxt = "n", womenpredict1$fit*100, type = "l", ylim = c(60, 100), main = "Hostile Sexism", ylab = "Probability of Trump Vote", cex.main = 1)
mtext("More Hostile                           Less Hostile", 1, line = .75, outer = FALSE, cex = .65)
arrows(7, 52.25, 6.2, 52.25, length = 0.05, xpd = TRUE)
arrows(7, 52.25, 7.9, 52.1, length = 0.05, xpd = TRUE)
points(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), womenpredict1$fit*100, pch = 16)
lines(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), womenpredict1$fit*100 + 1.96*womenpredict1$se.fit*100, lty = 2)
lines(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), womenpredict1$fit*100 - 1.96*womenpredict1$se.fit*100, lty = 2)


plot(c(1, 2, 3, 4, 5), womenpredict2$fit*100, type = "l", ylim = c(50, 100), main = "Feminism", xaxt = "n", ylab = "Probability of Trump Vote", cex.main = 1)
mtext("Less Feminist                           More Feminist", 1, line = .75, outer = FALSE, cex = .65)
arrows(3, 40.6, 2.69, 40.6, length = 0.05, xpd = TRUE)
arrows(3, 40.6, 3.27, 40.6, length = 0.05, xpd = TRUE)
points(c(1, 2, 3, 4, 5), womenpredict2$fit*100, pch = 16)
lines(c(1, 2, 3, 4, 5), womenpredict2$fit*100 + 1.96*womenpredict2$se.fit*100, lty = 2)
lines(c(1, 2, 3, 4, 5), womenpredict2$fit*100 - 1.96*womenpredict2$se.fit*100, lty = 2)


plot(c(1, 2, 3, 4, 5), womenpredict3$fit*100, type = "l", ylim = c(0, 100), main = "The Trump Video", xaxt = "n", ylab = "Probability of Trump Vote", cex.main = 1)
mtext("Should Have Mattered Less                           Should Have Mattered More", 1, line = .75, outer = FALSE, cex = .65)
arrows(3, -19.5, 2.7, -19.5, length = 0.05, xpd = TRUE)
arrows(3, -19.5, 3.25, -19.5, length = 0.05, xpd = TRUE)
points(c(1, 2, 3, 4, 5), womenpredict3$fit*100, pch = 16)
lines(c(1, 2, 3, 4, 5), womenpredict3$fit*100 + 1.96*womenpredict3$se.fit*100, lty = 2)
lines(c(1, 2, 3, 4, 5), womenpredict3$fit*100 - 1.96*womenpredict3$se.fit*100, lty = 2)


plot(c(1, 2, 3, 4, 5, 6, 7), womenpredict4$fit*100, type = "l", ylim = c(0, 100), main = "Trump's Treatment of Women", xaxt = "n", ylab = "Probability of Trump Vote", cex.main = .9)
mtext("Extremely Well                           Extremely Poor", 1, line = .75, outer = FALSE, cex = .65)
arrows(4, -19.5, 3.57, -19.5, length = 0.05, xpd = TRUE)
arrows(4, -19.5, 4.44, -19.5, length = 0.05, xpd = TRUE)
points(c(1, 2, 3, 4, 5, 6, 7), womenpredict4$fit*100, pch = 16)
lines(c(1, 2, 3, 4, 5, 6, 7), womenpredict4$fit*100 + 1.96*womenpredict4$se.fit*100, lty = 2)
lines(c(1, 2, 3, 4, 5, 6, 7), womenpredict4$fit*100 - 1.96*womenpredict4$se.fit*100, lty = 2)



# TRUMP'S LOST GENERAL VOTES BY GENDER ATTITUDES --------


frvoters <- subset(anes, female == 1 & vote16 == 1 & pid2_r == 1)

womenvideo <- c(table(frvoters$mattervideo))
womenvideo.perc <- c(prop.table(table(frvoters$mattervideo)))*100
womenpredict.video <- c(womenpredict3$fit)


trumpvoters.video <- data.frame(cbind(womenvideo, womenvideo.perc, womenpredict.video))
trumpvoters.video
trumpvoters.video$lostvotes <- womenvideo.perc * (1 - womenpredict.video)
trumpvoters.video$wonvotes <- womenvideo.perc * womenpredict.video
trumpvoters.video

counts <- rbind(trumpvoters.video$lostvotes, trumpvoters.video$wonvotes)
rownames(counts) <- c("Votes Lost", "Votes Won")

attach(mtcars)
par(mfrow=c(2,1),
    mar = c(3, 4, 4, 1))
par(family = "serif") #Set text

bp <- barplot(as.matrix(counts), ylab = "Percentage", names.arg = c("1\nNot at All", "2\n ", "3\n ", "4\n ", "5\nA Great Deal"), main = "Perception of How Much the Trump Video Should Have Mattered", col = c("gray37", "lightgray"), space = c(0, .2, .2, .2, .8), ylim = c(0, 42), cex.main = .9, cex.names = .9)
text(bp, trumpvoters.video$lostvotes + trumpvoters.video$wonvotes + .75, c(" 31.8%", " 32.3%", " 18.4%", " 4.7%", " 1.3%"), cex = .6)
text(bp, trumpvoters.video$lostvotes + .75, c(" 0.1%", " 0.8%", " 2.4%", " 3.3%", " 4.9%"), cex = .6)
abline(h = 0, lwd = 2)
lines(c(5, 5), c(0,35), lwd = 1.5, lty =3)
legend("topleft", c("Votes Won", "Votes Lost", "Cutoff for Electoral Advantage"), pch = c(22, 22, 26), lty = c(0, 0, 3), lwd = c(0, 0, 1),  col = c("black", "black"), pt.bg = c("lightgray", "gray37"), bty = "n", cex = .9)



womentreat <- c(table(frvoters$repwomen.rc))
womentreat.perc <- c(prop.table(table(frvoters$repwomen.rc)))*100
womenpredict.women <- c(womenpredict4$fit)


trumpvoters.women <- data.frame(cbind(womentreat, womentreat.perc, womenpredict.women))
trumpvoters.women
trumpvoters.women$lostvotes <- womentreat.perc * (1 - womenpredict.women)
trumpvoters.women$wonvotes <- womentreat.perc * womenpredict.women
trumpvoters.women

counts <- rbind(trumpvoters.women$lostvotes, trumpvoters.women$wonvotes)
rownames(counts) <- c("Votes Lost", "Votes Won")


bp <- barplot(as.matrix(counts), ylab = "Percentage", names.arg = c("1\nWell", "2\n ", "3\n ", "4\n ", "5\n ", "6\n ", "7\nPoor"), main = "Perception of Trump's Treatment of Women", col = c("gray37", "lightgray"), space = c(0, .2, .2, .2, .2, .2, .8), ylim = c(0, 50), cex.main = .9, cex.names = .9)
text(bp, trumpvoters.women$lostvotes + trumpvoters.women$wonvotes + .8, c(" 14.1%", " 26.2%", " 1.5%", " 36.5%", " 2.8%", " 5.1%", " 2.5%"), cex = .6)
text(bp, trumpvoters.women$lostvotes + .8, c(" 0.02%", " 0.1%", " 0.03%", " 2.1%", " 0.5%", " 3.2%", " 5.2%"), cex = .6)
abline(h = 0, lwd = 2)
lines(c(7.4, 7.4), c(0,38), lwd = 1.5, lty =3)
legend("topleft", c("Votes Won", "Votes Lost", "Cutoff for Electoral Advantage"), pch = c(22, 22, 26), lty = c(0, 0, 3), lwd = c(0, 0, 1),  col = c("black", "black"), pt.bg = c("lightgray", "gray37"), bty = "n", cex = .9)




# PARTISAN BIAS IN PERCEPTIONS OF TRUMP? --------------------

women <- subset(anes, female == 1 & vote16 = 1)

m1 <- polr(as.factor(mattervideo) ~ pid2_r + income + education + age + married + attend + ideology_c + sexism_hostile.rc + pid2_r * sexism_hostile.rc, data = women)
summary(m1) #Present
newdata1 <- data.frame(pid2_r = 1, married = mean(women$married, na.rm = TRUE), ideology_c = mean(women$ideology_c, na.rm = TRUE), education = mean(women$education, na.rm = TRUE), income = mean(women$income, na.rm = TRUE), age = mean(women$age, na.rm = TRUE), attend = mean(women$attend, na.rm = TRUE), sexism_hostile.rc = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
newdata2 <- data.frame(pid2_r = 0, married = mean(women$married, na.rm = TRUE), ideology_c = mean(women$ideology_c, na.rm = TRUE), education = mean(women$education, na.rm = TRUE), income = mean(women$income, na.rm = TRUE), age = mean(women$age, na.rm = TRUE), attend = mean(women$attend, na.rm = TRUE), sexism_hostile.rc = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
rvideo <- predict(m1, newdata1, type = "probs", se.fit = TRUE)
dvideo <- predict(m1, newdata2, type = "probs", se.fit = TRUE)
rvideo
dvideo


m2 <- polr(as.factor(repwomen.rc) ~ pid2_r + income + education + age + married + attend + ideology_c + sexism_hostile.rc + pid2_r * sexism_hostile.rc, data = women)
summary(m2) #Present
newdata1 <- data.frame(pid2_r = 1, married = mean(women$married, na.rm = TRUE), ideology_c = mean(women$ideology_c, na.rm = TRUE), education = mean(women$education, na.rm = TRUE), income = mean(women$income, na.rm = TRUE), age = mean(women$age, na.rm = TRUE), attend = mean(women$attend, na.rm = TRUE), sexism_hostile.rc = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
newdata2 <- data.frame(pid2_r = 0, married = mean(women$married, na.rm = TRUE), ideology_c = mean(women$ideology_c, na.rm = TRUE), education = mean(women$education, na.rm = TRUE), income = mean(women$income, na.rm = TRUE), age = mean(women$age, na.rm = TRUE), attend = mean(women$attend, na.rm = TRUE), sexism_hostile.rc = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
rwomen <- predict(m2, newdata1, type = "probs", se.fit = TRUE)
dwomen <- predict(m2, newdata2, type = "probs", se.fit = TRUE)
rwomen
dwomen


attach(mtcars)
par(mfrow=c(2,1),
    mar = c(3, 4, 4, 1))
par(family = "serif") #Set text


plot(rvideo[,5]*100, type = "l", ylim = c(0, 60), lty = 1, xaxt = "n", ylab = "Probability", main = "Perception that the Trump Video Should Have Mattered Much More", cex.lab = .85, cex.main = .9)
lines(dvideo[,5]*100, lty = 2)
points(rvideo[,5]*100, pch = 16)
points(dvideo[,5]*100)
mtext("More Hostile Sexism                           Less Hostile Sexism", 1, line = .75, outer = FALSE, cex = .85)
arrows(7, -8.25, 5.85, -8.25, length = 0.05, xpd = TRUE)
arrows(7, -8.25, 8.20, -8.25, length = 0.05, xpd = TRUE)
legend("topleft", c("Democrats", "Republican"), pch = c(21, 16), lty = c(2, 1), bty = "n", cex = .85)




plot(rwomen[,7]*100, type = "l", ylim = c(0, 70), main = "Perception that Trump Treats Women Extremely Poorly", lty = 1, xaxt = "n", ylab = "Probability", cex.lab = .85, cex.main = .9)
lines(dwomen[,7]*100, lty = 2)
points(rwomen[,7]*100, pch = 16)
points(dwomen[,7]*100)
mtext("More Hostile Sexism                           Less Hostile Sexism", 1, line = .75, outer = FALSE, cex = .85)
arrows(7, -9.6, 5.85, -9.6, length = 0.05, xpd = TRUE)
arrows(7, -9.6, 8.20, -9.6, length = 0.05, xpd = TRUE)
legend("topleft", c("Democrats", "Republican"), pch = c(21, 16), lty = c(2, 1), bty = "n", cex = .85)





# DID WOMEN ABSTAIN ----------------------------


frvoters <- subset(anes, female == 1 & pid2_r == 1)
frvoters1 <- subset(anes, female == 1 & pid2_r == 1 & who16_trump == 1)
ftrumpvoters <- subset(anes, (female == 1 & pid2_r == 1 & who16_trump == 1) | abstainer_r == 1)

m1 <- glm(abstainer_r ~ pid7_r + income + education + age + married + attend, family=binomial(link="logit"), data = frvoters)
summary(m1) #Present

m2 <- glm(abstainer_r ~ pid7_r + income + education + age + married + attend + sexism_hostile.rc, family=binomial(link="logit"), data = frvoters)
summary(m2) #Present

m3 <- glm(abstainer_r ~ pid7_r + income + education + age + married + attend + femscale, family=binomial(link="logit"), data = frvoters)
summary(m3) #Present

m4 <- glm(abstainer_r ~ pid7_r + income + education + age + married + attend + mattervideo, family=binomial(link="logit"), data = frvoters)
summary(m4)

m5 <- glm(abstainer_r ~ pid7_r + income + education + age + married + attend + repwomen.rc, family=binomial(link="logit"), data = frvoters)
summary(m5) #Present
newdata1 <- data.frame(pid7_r = mean(frvoters$pid7_r, na.rm = TRUE), married = mean(frvoters$married, na.rm = TRUE), education = mean(frvoters$education, na.rm = TRUE), income = mean(frvoters$income, na.rm = TRUE), age = mean(frvoters$age, na.rm = TRUE), attend = mean(frvoters$attend, na.rm = TRUE), repwomen.rc = c(1,2,3,4,5,6,7))
womenpredict1b <- predict(m5, newdata1, type = "response")


actual <- (420) / (420+72)
actual

frvoters <- subset(anes, female == 1 & pid2_r == 1 & abstainer_r == 1)
abstainers <- prop.table(table(frvoters$abstainer_trump))
abstainers

change1 <- (420-(122*.38)) / (420+122+72)
change1



# CAUSAL MEDIATION ANALYSIS OF ATTITUDES AND EVALUATIONS ON VOTE C --------

library(mediation)
library(sandwich)

meddat <- subset(anes, female == 1 & (who16_trump == 1 | who16_trump == 0))

m1 <- polr(factor(mattervideo) ~ age + income + education + attend + pid7_r + femscale, data = meddat)
summary(m1)

m2 <- glm(who16_trump ~ age + income + education + attend + pid7_r + femscale + mattervideo, family = binomial(probit), data = meddat)
summary(m2)

med1 <- mediate(model.m = m1, model.y = m2, treat = "pid7_r", mediator = "mattervideo", data = meddat, robustSE = TRUE)
summary(med1)

