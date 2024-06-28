

# age prediction
# without controlling gender
m1<-multinom(factor(age)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words, 
        data=df) 

m2 <- multinom(factor(age)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words 
            +adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions, 
        data=df) 


m3 <- multinom(factor(age)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities +  personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component, 
        data=df) 


m4 <- multinom(factor(age)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW, 
        data=df) 


m5 <- multinom(factor(age)~flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW +
            mtld_original_aw + mattr50_aw + hdd42_aw, 
        data=df)


# control for gender

m1<-multinom(factor(age)~factor(label) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words , 
        data=df) 

m2 <- multinom(factor(age)~factor(label) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words 
            +adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions, 
        data=df) 


m3 <- multinom(factor(age)~factor(label) +flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities +  personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component, 
        data=df) 


m4 <- multinom(factor(age)~factor(label) +flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW, 
        data=df) 


m5 <- multinom(factor(age)~factor(label) + flesch + gunningFog + characters_per_word + syll_per_word + ttr + long_words +
            adverbs + adjectives + auxiliary + named_entities + personal_pronouns + subordinate_conjunctions +
            action_component + affect_friends_and_family_component + certainty_component + economy_component + failure_component + fear_and_digust_component + joy_component + negative_adjectives_component + objects_component + polarity_nouns_component + polarity_verbs_component + politeness_component + positive_adjectives_component + positive_nouns_component + positive_verbs_component + respect_component + social_order_component + trust_verbs_component + virtue_adverbs_component + well_being_component + COCA_spoken_Bigram_Frequency+ COCA_spoken_Frequency_AW+ COCA_spoken_Range_AW + COCA_spoken_bi_MI2 + All_AWL_Normed + WN_Mean_Accuracy + LD_Mean_Accuracy + LD_Mean_RT + MRC_Familiarity_AW + MRC_Imageability_AW + Brysbaert_Concreteness_Combined_AW +
            mtld_original_aw + mattr50_aw + hdd42_aw, 
        data=df)
        


# funtion of extracting coefficients
pValue_extract <- function(x){
  z <- summary(x)$coefficients/summary(x)$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  p
}


coefficients_en_age <- coef(m5)

coefficients_blog_age <- coef(m5)

coefficients_es_age <- coef(m2)


# pan13-en

# Extract coefficients for 20s and 30s compared to 10s
coeff_20s_vs_10s <- coefficients_en_age[1, ]  # Change index as needed
coeff_30s_vs_10s <- coefficients_en_age[2, ]  # Change index as needed




# Create a data frame for the coefficients with variable names
coef_data_age_en <- data.frame(Coefficient = c(coeff_20s_vs_10s, coeff_30s_vs_10s),
                        Features = colnames(coefficients_en_age),
                        prob = c((exp(coeff_20s_vs_10s) / (1+exp(coeff_20s_vs_10s))), (exp(coeff_30s_vs_10s) / (1+exp(coeff_30s_vs_10s)))),
                        Age = rep(c("20s vs 10s", "30s vs 10s"), each = ncol(coefficients_en_age)),
                        p = c(pValue_extract(m5)[1,], pValue_extract(m5)[2,]))

coef_data_age_en$gender <- coef_data_age_en$prob < 0.5
coef_data_age_en$gender <-  ifelse(coef_data_age_en$gender == TRUE,'male', 'female')

filter_en_age <- coef_data_age_en %>% filter(p <0.05 )

filter_en_age$feature_group <- c('gender(female)', rep('surface', 4), rep('syntactic',4), rep('sentiment', 7), rep('lexical_sophistication', 5), rep('lexical_diversity',1), rep('syntactic',3), rep('sentiment', 4))
filter_en_age$data <- rep('PAN13-EN', nrow(filter_en_age))
en_age_sf <- filter_en_age$Features


# blog

# Extract coefficients for 20s and 30s compared to 10s
coeff_20s_vs_10s <- coefficients_blog_age[1, ]  # Change index as needed
coeff_30s_vs_10s <- coefficients_blog_age[2, ]  # Change index as needed




# Create a data frame for the coefficients with variable names
coef_data_age_blog <- data.frame(Coefficient = c(coeff_20s_vs_10s, coeff_30s_vs_10s),
                        Features = colnames(coefficients_blog_age),
                        prob = c((exp(coeff_20s_vs_10s) / (1+exp(coeff_20s_vs_10s))), (exp(coeff_30s_vs_10s) / (1+exp(coeff_30s_vs_10s)))),
                        Age = rep(c("20s vs 10s", "30s vs 10s"), each = ncol(coefficients_blog_age)),
                        p = c(pValue_extract(m5)[1,], pValue_extract(m5)[2,]))

coef_data_age_blog$gender <- coef_data_age_blog$prob < 0.5
coef_data_age_blog$gender <-  ifelse(coef_data_age_blog$gender == TRUE,'male', 'female')

filter_blog_age <- coef_data_age_blog %>% filter(p <0.05 )

filter_blog_age <- filter_blog_age[c(-1,-16),]
filter_blog_age$feature_group <- c('gender(female)', rep('surface', 5), rep('syntactic',5), rep('lexical_sophistication', 3), 'gender(female)',rep('surface', 4), rep('syntactic',5), rep('sentiment', 1), rep('lexical_sophistication', 2))
filter_blog_age$data <- rep('BLOG', nrow(filter_blog_age))

blog_age_sf <- filter_blog_age$Features


# pan13-es

# Extract coefficients for 20s and 30s compared to 10s
coeff_20s_vs_10s <- coefficients_es_age[1, ]  # Change index as needed
coeff_30s_vs_10s <- coefficients_es_age[2, ]  # Change index as needed




# Create a data frame for the coefficients with variable names
coef_data_age_es <- data.frame(Coefficient = c(coeff_20s_vs_10s, coeff_30s_vs_10s),
                        Features = colnames(coefficients_es_age),
                        prob = c((exp(coeff_20s_vs_10s) / (1+exp(coeff_20s_vs_10s))), (exp(coeff_30s_vs_10s) / (1+exp(coeff_30s_vs_10s)))),
                        Age = rep(c("20s vs 10s", "30s vs 10s"), each = ncol(coefficients_es_age)),
                        p = c(pValue_extract(m2)[1,], pValue_extract(m2)[2,]))

coef_data_age_es$gender <- coef_data_age_es$prob < 0.5
coef_data_age_es$gender <-  ifelse(coef_data_age_es$gender == TRUE,'male', 'female')

filter_es_age <- coef_data_age_es %>% filter(p <0.05 )

filter_es_age <- filter_es_age[c(-1,-11),]

es_age_sf <- filter_es_age$Features