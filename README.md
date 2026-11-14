# What Can Go Wrong in Authorship Profiling: Cross-Domain Analysis of Gender and Age Prediction

This repository accompanies the paper [What Can Go Wrong in Authorship Profiling: Cross-Domain Analysis of Gender and Age Prediction](https://aclanthology.org/2024.gebnlp-1.9/) (Chen et al., GeBNLP 2024).

## About

This project studies how consistent authorship profiling models predict gender and age across datasets across domains.

We compare classical classifiers (logistic regression, decision tree, random forest, and Naive Bayes) with transformer models (BERT, RoBERTa, and XLNet).

We sample data from the original publicly available PAN13 English, PAN13 Spanish, and Blog Authorship Corpus (BLOG) datasets.

## Scripts

- `scripts/preprocess`: data preparation and preprocessing
- `scripts/classifiers + evaluate`: training and evaluation of gender and age classifiers
- `analysis/regression_analysis`: stylistic feature extraction and analysis
- `analysis/topic`: analysis of topical bias and prediction errors

## Responsible use

The datasets use binary gender labels and do not capture the full range of gender identities. Model predictions and dataset-specific patterns should not be treated as universal differences in how people of different genders or ages write.

## Contact

Questions: `hongyu.chen@iris.uni-stuttgart.de`
