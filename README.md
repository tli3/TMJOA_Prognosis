# Input, output and script description.

Step1: The script is step1_single.r. The input is "TMJOAI_Long_040422_Norm.csv", which includes the response (y) (first column) and standardized features/covariates X (all the other columns).
The output will include the predicted responses (y) in "out/" folder as independent test set, and the predicted responses (y) in "out_valid/" folder as validation data set.
other outputs will also include the importance scores of all features in imp/ folder, the number of selected features for each test and training set, in the select/ folder, and the SHAP value in the SHAP/ folder.
The first line of step 1 is iii=1. But the sucessful implementation of step 1 is that not only iii=1 can run successfully but also iii=2,3,4,...48 can all run successfully, with 48 files in out/ and 48 files in out_valid/ folders being generated successfully.

Step2: the script is Step2_EHMN.r. The input is the all predicted responses in "out/" folder which are the output in Step 1 (after you run iii=1,2,...48). 


# Method description.

To avoid overfitting, we employed the nested 10-fold CV method to build and evaluate the
performance of various predictive models. Our method consisted of two nested CV loops, each
implementing a 10-fold CV. The outer loop aimed to provide an unbiased evaluation of model
performance, while the inner loop determined the hyperparameters for the final model. Specifically,
all subjects were split into 10 folds. One fold was kept as an independent test set,
and the remaining folds were further split into 10 subfolds. One subfold ௜ǡ௝ was considered an validation set and the remaining subfolds were considered as the training
set. We trained various statistical and machine learning models using the training dataset. The
validation dataset was utilized to adjust the hyperparameters and determine the number of top
features. In the inner loop of the nested CV, the validation dataset looped over 10 subfolds,
and the model trained on the training data was applied to predict the outcome of the
validation set. In the outer loop of the nested CV, the test set looped over the 10 folds. The 34 OA patients underwent the nested 10-fold CV process, while the 40
normal controls were added as additional training resources to the training data during each loop of
the cross-validation.

We tested six feature selection (FS) methods including:FS1) selection frequency of LASSO (Glmnet),
FS2) permutation importance for Random Forest (RF), FS3) gain for XGboost (XGboost), FS4)
combinations of the absolute values of weights for neural network (NNET), FS5) absolute value of
coefficients in Glmboost (Glmboost), FS6) AUC between each feature and the response (AUC). We
evaluated eight predictive modeling (PM) methods including:PM1) elastic net (Glmnet), PM2)
Glmboost, PM3) High-Dimensional Discriminant Analysis (HDDA), PM4) single-hidden-layer neural
networks (NNET), PM5) RF, PM6) XGBoost, PM7) Kernel-based Support Vector Machine (SVM),
and PM8) Linear Discriminant Analysis (LDA). In total there are 6 FS * 8 PM = 48 machine learning
methods.
For each model, we employed three major steps on the training dataset: 1) calculation of feature
importance scores based on FS1-FS6 methods, respectively, in the inner loop, 2) ranking of feature
importance scores and selection of top features to train the ML models, 3) using the selected number
of top features to train the model with the training and validation datasets together. Then, we
evaluated the trained models’ performances on the test set (outer loop of the nested CV). All feature
selection, machine learning predictive modeling based on nested CV were carried out using the
package “caret” of R/4.1.0 software, and the six feature importance scores were calculated by the
intrinsic metrics of the corresponding ML methods in the package “caret”, respectively.
We proposed a method called Ensemble via Hierarchical Predictions through the Nested CV (EHPN)
to improve predictive performance. This method combines 18 models with optimal predictive
performance based on the validation dataset, six FS models with PM2, six FS models with PM3, and
six FS models with PM8, as PM2, PM3 and PM8 were the top three performing ML methods. We
carried out the model ensemble on the validation dataset; specifically, a Glmboost model was trained
to assign different weights to the 18 models during the validation step. We evaluated the performance
of the combined model using the test dataset. Since the test set loops over the 10 folds, the Glmboost
model was trained with different and fold-specific set each time.

