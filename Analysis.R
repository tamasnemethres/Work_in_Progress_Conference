#Nullmodel
tom0 <- glm(ToM ~ 1, data= tom, family= "binomial")
#First modell
tom1 <- glm(ToM ~ Age, data= tom, family = "binomial")
anova(tom0, tom1, test= "LRT")
CIbinm2(tom1)
summary(tom1)
#Adding grouping 
tom1.1 <- glm(ToM ~ Age+ grouping_new, data= tom, family = "binomial")
anova(tom1, tom1.1, test= "LRT")
emmeans(tom1.1, list(pairwise ~ grouping_new), type= "response")

#removing the missing variables, otherwise the sample wouldn't be even in the two models. 
tom_c <- na.omit(tom)
tom1.1_c <-  glm(ToM ~ Age+ grouping_new, data= tom_c, family= "binomial")
#Adding device usage 
tom1.1.2 <- glm(ToM ~ Age+ grouping_new+Device_min_per_day, data= tom_c, family= "binomial")
anova(tom1.1_c, tom1.1.2, test= "LRT")



#Creating a dataset for pre-Covid
Pre_Covid <- tom %>%
  filter(grouping_new == "Pre-Covid")

describe(Pre_Covid$Age)

#Creating a dataset for Covid
During_Covid <- tom %>%
  filter(grouping_new == "During-Covid")

describe(During_Covid$Age)

#Creating a dataset for post-Covid
After_Covid <- tom %>%
  filter(grouping_new == "After-Covid")

describe(After_Covid$Age)

#Pre-Covid model
model_0_pc <- glm(ToM ~ 1, data=Pre_Covid, family= "binomial")
model_pc <- glm(ToM ~ Age, data=Pre_Covid, family= "binomial")
anova(model_0_pc, model_pc, test= "LRT")



#Covid model
model_0_dc <- glm(ToM ~ 1 , data= During_Covid, family="binomial")
model_dc <- glm(ToM ~ Age, data=During_Covid, family= "binomial")
anova(model_0_dc, model_dc, test="LRT")
summary(model_dc)

CIbinm2(model_dc)

#post-Covid model
model_0_ac <-glm(ToM ~ 1 , data= After_Covid, family="binomial")
model_ac <- glm(ToM ~ Age, data=After_Covid, family= "binomial")
anova(model_0_ac, model_ac, test="LRT")

#Normalioty test
by(tom$Age, tom$grouping_new, function(x){shapiro.test(x)})


#Homogenity of Variances test
leveneTest(tom$Age, tom$grouping_new)

#rang-Welch test
wrs2 <- t1way(Age ~ grouping_new, data= tom, tr= 0.2)
print(wrs2)

#Significance level of the test
p_value <- wrs2$p.value
print(p_value) # because wrs2 gave not an exact p value

#Post-Hoc test with Bonferroni correction
post_hoc_bon <- lincon(Age ~grouping_new, data = tom,method = "bonferroni")

print(post_hoc_bon)



# Classification tree
tree <- rpart(ToM ~ Age, data=tom, method= "class")

################################################################################
#Second-order Theory of Mind task
################################################################################


#Nullmodel
tom2nd_0 <- glm(ToM_2nd ~ 1, data=tom2nd_filtered, family= "binomial")

#Adding Age
tom2nd_1 <- glm(ToM_2nd ~ Age, data=tom2nd_filtered, family= "binomial")

anova(tom2nd_0,tom2nd_1, test= "LRT")
CIbinm2(tom2nd_1)
summary(tom2nd_1)

#Adding grouping
tom2nd_1.1 <- glm(ToM_2nd ~ Age+ grouping_new, data=tom2nd_filtered, family= "binomial")
#Comparing the change between the model only with Age, and the model with grouping
anova(tom2nd_1,tom2nd_1.1, test= "LRT")

emmeans(tom2nd_1.1, list(pairwise ~ grouping_new), type= "response")
summary(tom2nd_1.1)

#Cleaning the data to be comparable the two models
tom2nd_filtered_cleaned <- na.omit(tom2nd_filtered)

tom2nd_1.2 <- glm(ToM_2nd ~ Age+ grouping_new, data=tom2nd_filtered_cleaned, family= "binomial")
#Adding device use
tom2nd_1.1.1 <- glm(ToM_2nd ~ Age+ grouping_new+ Device_min_per_day, data=tom2nd_filtered_cleaned, family= "binomial")

anova(tom2nd_1.2, tom2nd_1.1.1, test= "LRT")





#How Age and grouping related
age_0 <- lm(Age ~1, data= tom2nd_filtered)
age_lm <- lm(Age ~ grouping_new, data = tom2nd_filtered)
anova(age_0, age_lm, test= "LRT")




summary(age_lm)






#Creating a dataset for Covid
During_Covid_2 <- tom2nd %>%
  filter(grouping_new == "During-Covid")

#Creating a dataset for post-Covid
After_Covid_2 <- tom2nd %>%
  filter(grouping_new == "After-Covid")



#Covid model
model_0_dc <- glm(ToM_2nd ~ 1 , data= During_Covid_2, family="binomial")
model_dc <- glm(ToM_2nd ~ Age, data=During_Covid_2, family= "binomial")
anova(model_0_dc, model_dc, test="LRT")
summary(model_dc)

CIbinm2(model_dc)

#post-Covid model
model_0_ac <-glm(ToM_2nd ~ 1 , data= After_Covid_2, family="binomial")
model_ac <- glm(ToM_2nd ~ Age, data=After_Covid_2, family= "binomial")
anova(model_0_ac, model_ac, test="LRT")

summary(model_ac)

CIbinm2(model_ac)









######################################################################################
#Real-Apparent Emotions
######################################################################################
################################################################################
#Descriptive statistics
################################################################################
table(appenreal$Test_date)
View(appenreal)
psych::describe(appenreal$Device_min_per_day)
describeBy(appenreal$Device_min_per_day, group= appenreal$grouping_new)
psych::describe(appenreal$Age)
table(appenreal$Gender)
table(appenreal$grouping_new)
describeBy(appenreal$Age, group= appenreal$grouping_new)
################################################################################
#Analysis
################################################################################
#nullmodel
appen_0 <- glm(Appen_r_a ~ 1, data= appenreal, family= "binomial")
#Effect of Age on the Real-Apparent emotion task
appen_1 <- glm(Appen_r_a ~ Age, data= appenreal, family= "binomial")

anova(appen_0, appen_1, test= "LRT")

#The real apparent emotion ndevelopment was not realted to Age, I removed it from the model
appen_2 <- glm(Appen_r_a ~ grouping_new, data= appenreal, family= "binomial")
anova(appen_0, appen_2, test= "LRT")
emmeans(appen_2, list(pairwise ~ grouping_new), type= "response")

#Cleaning the data, to be even the sample size in the two models
appen_c <- na.omit(appenreal)
appen_2_c <- glm(Appen_r_a ~ grouping_new, data= appen_c, family= "binomial")
appen_2.1_c <- glm(Appen_r_a ~ grouping_new+Device_min_per_day, data= appen_c, family= "binomial")
anova(appen_2_c, appen_2.1_c, test="LRT")


#Normality test
by(appenreal$Age, appenreal$grouping_new, function(x){shapiro.test(x)})

#Analysis of variances test
leveneTest(appenreal$Age, appenreal$grouping_new)

#rang-Welch test
wrs2 <- t1way(Age ~ grouping_new, data= appenreal, tr= 0.2)
print(wrs2)



#post-hoc test
post_hoc_bon <- lincon(Age ~grouping_new, data = appenreal,method = "bonferroni")

print(post_hoc_bon)


################################################################################
#Appenreal-emotions
################################################################################

#The emotion recognition contained missing data, a cleaned dataset was created to be even the sample sizes across the models 
appen_emo_c <-  na.omit(appen_emo)

#is emotion recognition development predicting Real-Apparent Emotion recognition development? 
model_appenemo_0 <- glm(Appen_r_a ~ 1, data= appen_emo_c, family= "binomial")
model_appenemo_1 <- glm(Appen_r_a ~ Emo_recog_per, data= appen_emo_c, family= "binomial")
anova(model_appenemo_0,model_appenemo_1, test="LRT")

