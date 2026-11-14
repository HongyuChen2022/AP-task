library(tidyverse)
library(tidymodels)
library(AER)
library(stargazer)
library(titanic)
library(car)
library(lmtest)
library(kableExtra, lib.loc = .libPaths()[1])
library(readr)
library(modelr)
library(nnet)



df <- read_csv("~/Desktop/data/merged_features/scaled_merged.csv")

# aggregating features for lr models predicting gender
# models without controlling age
m1<-glm(factor(label)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words, data=df, family="binomial") 
m2 <- glm(factor(label)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +adverbs + adjectives + auxiliary + named_entities + past_tense + personal_pronouns + subordinate_conjunctions, data=df, family="binomial") 
m3 <- glm(factor(label)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words + adverbs + adjectives + auxiliary + named_entities + past_tense + personal_pronouns + subordinate_conjunctions + action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component, data=df, family="binomial") 
m4 <- glm(factor(label)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + past_tense + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW, 
        data=df, family="binomial") 

m5 <- glm(factor(label)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + past_tense + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW +
            mtld_original_aw + mattr50_aw + hdd42_aw, 
        data=df, family="binomial") 


# models with age controlled

m1<-glm(factor(label)~factor(age) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words, 
        data=df, family="binomial") 

m2 <- glm(factor(label)~factor(age) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words 
            +adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions, 
        data=df, family="binomial") 

m3 <- glm(factor(label)~factor(age) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component, 
        data=df, family="binomial") 
m4 <- glm(factor(label)~factor(age) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW, 
        data=df, family="binomial") 
m5 <- glm(factor(label)~ factor(age) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + past_tense + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW +
            mtld_original_aw + mattr50_aw + hdd42_aw, 
        data=df, family="binomial") 


# calculting BIC score for both models (age controlled/non-controlled)

fits<-lapply(list(m1=m1,m2=m2, m3=m3, m4=m4, m5=m5),BIC) %>% bind_rows(.id="model")
fits

# analysis with the best model: model 5 with age controlled
# Extract coefficients
coef_values <- coef(m5)

# Create a data frame
coef_df_en <- data.frame(variable = names(coef_values), prob = exp(coef_values)/ (1 + exp(coef_values)))

# Get p-values for each predictor
p_values <- summary(
m5)$coefficients[, "Pr(>|z|)"]

# Set a significance level (e.g., 0.05)
significance_level <- 0.05

# Filter significant variables based on p-values
significant_variables_en <- names(p_values[p_values < significance_level])

# Display the names of significant variables
cat("Significant Variables:", paste(significant_variables_en, collapse = ", "))






