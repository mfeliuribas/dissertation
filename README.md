# Dissertation

This is the repository for my dissertation. You will find the following folders and documents:

- study_1
    - data: final data + preliminary data (all of *cesa* + part of *stc*)
    - scripts: 'descriptives.R', 'descriptives_prelim_data.R'
- study_2
    - final_data: 3 .csv files
        - 'bls_final.csv': final dataset for bilinguals (Spanish and English)
        - 'english_final.csv': final dataset for English (bilinguals + monolinguals)
        - 'spanish_final.csv': final dataset for for Spanish (bilinguals + monolinguals)
    - models: 3 .rds and 5 csv. files
        - 'bls_model_0.rds': intercept-only model for bilinguals
        - 'en_model_0.rds': intercept-only model for English
        - 'mod_final_bls_post1.csv': posteriors from final bilingual model (categorical preds.)
        - 'mod_final_bls_post2.csv': posteriors from final bilingual model (form, blp_z)
        - 'mod_final_bls_post2.csv': posteriors from final bilingual model (form, language, lextale_avg_z)
        - 'mod_final_en_post.csv': posteriors from final English model
        - 'mod_final_sp_post.csv': posteriors from final Spanish model
        - 'sp_model_0.rds': intercept-only model for Spanish
    - raw_data: 5 .csv files
        - 'english_mls_raw.csv': raw data for English monolinguals
        - 'experiment_bls_raw.csv': raw data for experiment (bilinguals only)
        - 'lextale_bls_raw.csv': raw data for LexTale (bilinguals only)
        - 'questionnaire_bls_raw.csv': raw data for BLP (bilinguals only)
        - 'spanish_mls_raw.csv': raw data for Spanish monolinguals
    - scripts: 10 .R files
        - 'comparisons_bls.R': pairwise comparisons with the bilingual data.
        - 'comparisons_en.R': pairwise comparisons with the English data.
        - 'comparisons_sp.R': pairwise comparisons with the Spanish data.
        - 'descriptives.R': descriptive stats for the 'Methods' section.
        - 'final_datasets.R': code to create the final datasets.
        - 'models_bls.R': models with bilingual data.
        - 'models_en.R': models with English data.
        - 'models_sp.R': models with Spanish data.
        - 'tidy_data_bls.R': code to wrangle the bilingual data.
        - 'tidy_data_mls.R': code to wrangle the monolingual data.
    - tidy_data: 7 .csv files
        - 'english_mls_tidy.csv': tidy data for English monolinguals
        - 'experiment_bls_tidy.csv': tidy data for experiment (bilinguals only)
        - 'lextale_bls_tidy.csv': tidy data for lextale (bilinguals only)
        - 'lextale_en_bls_tidy.csv': tidy data for English lextale only (bilinguals only)
        - 'lextale_sp_bls_tidy.csv': tidy data for Spanish lextale only (bilinguals only)
        - 'questionnaire_bls_tidy.csv': tidy data for BLP (bilinguals only)
        - 'spanish_mls_tidy.csv': tidy data for Spanish monolinguals
- study_3


Last updated: August 14, 2026

