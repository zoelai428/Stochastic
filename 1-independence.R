####################
###### Task 1 ######
####################

### b) ###
# show 2 plots next to each other
par(mfrow = c(1,2))

# get Mosaic-Plot for male students:
mosaicplot(HairEyeColor[,,1], main = "Male")

# get Mosaic-Plot for female students:
mosaicplot(HairEyeColor[,,2], main = "Female")

# reset to show 1 plot only
par(mfrow=c(1,1))

# get Mosaic-Plot for both male and female together:
mosaicplot(HairEyeColor, main = "Both Male and Female")



### c) ###

# Firstly, for male students:

# get relative frequency
male_relative <- prop.table(xtabs(Freq ~ Hair + Eye, data = HairEyeColor[,,1]))

# add both row and column sums
male_table <- addmargins(male_relative,  margin = 1:2)

# show the contingency table
male_table


# Similarly, for female students and altogether:

female_relative <- prop.table(xtabs(Freq ~ Hair + Eye, data = HairEyeColor[,,2]))
female_table <- addmargins(female_relative,  margin = 1:2)
female_table

maleandfemale_relative <- prop.table(xtabs(Freq ~ Hair + Eye, data = HairEyeColor))
maleandfemale_table <- addmargins(maleandfemale_relative, margin = 1:2)
maleandfemale_table



### d) ###

# Firstly, for male students:

# store the sums of each hair colour and eye colour in a vector respectively
male_hair_marginals = male_table[1:4,"Sum"]
male_eye_marginals = male_table["Sum",1:4]

# initialize the table for output
male_expected_prob <- xtabs(rep(0, times = 16) ~ Hair + Eye, data = HairEyeColor[,,1])

# get the expected probabilities
for (i in 1:4) {
  for (j in 1:4) {
    male_expected_prob[i, j] <- male_hair_marginals[i] * male_eye_marginals[j]
  }
}

# get the expected absolute frequencies
male_expected_count <- sum(HairEyeColor[,,1])*male_expected_prob
male_expected_count


# Similarly, for female students and altogether:

# for female students:
female_hair_marginals = female_table[1:4,"Sum"]
female_eye_marginals = female_table["Sum",1:4]

female_expected_prob <- xtabs(rep(0, times = 16) ~ Hair + Eye, data = HairEyeColor[,,2])

for (i in 1:4) {
  for (j in 1:4) {
    female_expected_prob[i, j] <- female_hair_marginals[i] * female_eye_marginals[j]
  }
}

female_expected_count <- sum(HairEyeColor[,,2])*female_expected_prob
female_expected_count

# both male and female together:
maleandfemale_hair_marginals = maleandfemale_table[1:4,"Sum"]
maleandfemale_eye_marginals = maleandfemale_table["Sum",1:4]

maleandfemale_expected_prob <- xtabs(rep(0, times = 32) ~ Hair + Eye, data = HairEyeColor)

for (i in 1:4) {
  for (j in 1:4) {
    maleandfemale_expected_prob[i, j] <- maleandfemale_hair_marginals[i] * maleandfemale_eye_marginals[j]
  }
}

maleandfemale_expected_count <- sum(HairEyeColor)*maleandfemale_expected_prob
maleandfemale_expected_count



### e) ###

# get the squared deviations between the observed values and the expected values
male_squared_deviations <- 
  (xtabs(Freq ~ Hair + Eye, data = HairEyeColor[,,1]) - male_expected_count)^2
male_squared_deviations

female_squared_deviations <- 
  (xtabs(Freq ~ Hair + Eye, data = HairEyeColor[,,2]) - female_expected_count)^2
female_squared_deviations

maleandfemale_squared_deviations <- 
  (xtabs(Freq ~ Hair + Eye, data = HairEyeColor) - maleandfemale_expected_count)^2
maleandfemale_squared_deviations



### f) ###

# apply the formula of the test statistic:
# T=\sum_k \sum_l \dfrac{(\text{observed}_{k,l}-\text{expected}_{k,l})^2}{\text{expected}_{k,l}}
# (Formula 3 in the accompanied writing)

# get the test statistic for male students:
T_male <- sum(male_squared_deviations / male_expected_count)
T_male

# get the test statistic for female students:
T_female <- sum(female_squared_deviations / female_expected_count)
T_female

# get the test statistic for both male and female students:
T_maleandfemale <- sum(maleandfemale_squared_deviations / maleandfemale_expected_count)
T_maleandfemale

# 0.95-Quantile of the Chi-squared distribution:
# degree of freedom = (No. hair colours - 1) * (No. of eye colours - 1) = 3*3 = 9
qchisq(p = 0.95, df = 9)



### g) ###

# get the p-values of chi^2-Test

# for male students:
pchisq(q = T_male, df = 9, lower.tail = FALSE)

# for female students:
pchisq(q = T_female, df = 9, lower.tail = FALSE)

# for both male and female students:
pchisq(q = T_maleandfemale, df = 9, lower.tail = FALSE)



### h) ###

# apply chisq.test() instead

# for male students:
chisq.test(HairEyeColor[,,1])

# for female students:
chisq.test(HairEyeColor[,,2])

# for both male and female students:
chisq.test(HairEyeColor[,,1] + HairEyeColor[,,2])



### i) ###

# get a table to show the absolute frequencies of each eye colour with respect to gender
eye_gender <- xtabs(Freq ~ Eye + Sex, data = HairEyeColor)

# test, if eye colour is identically distributed for male and female
chisq.test(eye_gender)

# get a table to show the absolute frequencies of each hair colour with respect to gender
hair_gender <- xtabs(Freq ~ Hair + Sex, data = HairEyeColor)

# test, if hair colour is identically distributed for male and female
chisq.test(hair_gender)
