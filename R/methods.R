#---------------------------------------------

#' Reject Option based Classification method
#' @aliases  Reject Option based Classification method
#' @author Leonardo Paes Vieira
#' @description Reject Option based Classification (ROC) method for discrimination reduction in predictive models. Given a probabilistic model
#' for binary classifications, ROC is a post processing method which changes instances' classification labels in a defined probability interval.
#' In a probabilistic model, the decision criteria is defined by simply choosing the category with the higher estimated probability.
#' Considering a binary classification, a model returns two complementary probabilities.
#'
#' Assuming that discriminatory classifications occurs near rejection boundary (when probabilities are near 0.5), the ROC method
#' defines an interval in which probabilities can be considered next to the boundary.
#' Then, once the interval size ([0.5, theta]) is defined, the method looks for the higher probability between the two classes.
#' If a privileged person receives a positive classification with probability between 0.5 and theta, the method turn this classification
#' to negative. Conversely, if the method finds a deprived person with negative classification probability between 0.5 and theta, then it changes her to positive.
#'
#' @details In a binary classification, the highest probability is always greater than 0.5. Considering already classified instances,
#' and selecting people from the privileged-positive classified group and deprived-negative classified group, the method searches for those
#' with the maximum probability less than theta.
#' In this case, the function will change the instance's classification label and replace the two probabilities with their complementary.
#' The user must run the data frame with predictions, the column name with the sensible attribute, as well as the privileged group name through the ROC method.
#' Also, the user must add the classification column name, the categories probabilities columns names and the name of the category considered the positive one.
#' This function returns a data frame with updated probabilities and classifications.
#'
#' @param pred_mod  data frame - predictions and its probabilities with respect to each category.
#' @param positive_col string - positive classification probabilities column name
#' @param negative_col string - negative classification probabilities column name
#' @param sensible_col string - sensible attribute column name
#' @param privileged_group string - privileged group label
#' @param classification_col string - classifications column name
#' @param positive_class string - positive classification label
#' @param theta  numeric - classification probabilities threshold
#'
#'
#' @return Returns a new data frame with updated classifications and probabilities, maintaining the structure (columns and its names)
#' of the original data frame, ran in the method.
#'
#' @references F. Kamiran, A. Karim and X. Zhang, "Decision Theory for Discrimination-Aware Classification," 2012 IEEE 12th International Conference on Data Mining, 2012, pp. 924-929, doi: 10.1109/ICDM.2012.45.
#'
#'
#'
#' @examples
#'
#'  data('adult.data')
#'
#'  adult.data$income = ifelse(test =  adult.data$income == '>50K',
#'                              yes = 1, no = 0)
#'
#'  adult.data = adult.data[, colnames(adult.data) %in%
#'                            c('age', 'education', 'sex',
#'                              'income', 'capital_gain')]
#'
#'  adult.data = adult.data[sample(1:nrow(adult.data), size = 100, replace = FALSE), ]
#'
#'  ##### Logistic Regression
#'
#'  if (!requireNamespace("stats", quietly = TRUE)) {
#'  stop("Package \"stats\" needed for this example to work.",
#'  call. = FALSE)}
#'
#'  mod = glm(formula =  income ~., data = adult.data, family = binomial(link = 'logit'))
#'
#'  ### The 'predict' function returns the classes probabilities
#'  ### automatically for caret (package) models
#'
#'  pred = data.frame(greater = mod$fitted.values, less =  1 - mod$fitted.values, sex = adult.data$sex,
#'                    classification = ifelse(mod$fitted.values >= 0.5, 'greater', 'less'))
#'
#'  theta = 0.6
#'
#'  pred_changed = roc_method(pred_mod = pred, positive_col = 'greater',
#'                           positive_class = 'greater', negative_col = 'less',
#'                           sensible_col = 'sex', privileged_group = 'Male',
#'                           classification_col = 'classification',
#'                           theta = theta)
#'
#'
#'  pred_changed

roc_method <- function(pred_mod, positive_col, positive_class, negative_col,
                       sensible_col, privileged_group, classification_col, theta){

  negative_class <- as.character(unique((pred_mod[pred_mod[classification_col] != positive_class,
                                                  classification_col])))



  if(length(negative_class) != 1){

    stop('Must be a binary classification.')

  }

  #### get rownames of privileged group

  lines_privileged = rownames(pred_mod[(pred_mod[sensible_col] == privileged_group) &
                                         (pred_mod[classification_col] == positive_class),])

  #### creating a dataframe only with privileged people

  privileged = pred_mod[rownames(pred_mod) %in% lines_privileged, ]

  ### if a privileged pearson gets a positive class with probability <= theta, then her/his classification turns negative

  privileged[[classification_col]] = ifelse(test = privileged[[positive_col]] <= theta,
                                          yes = negative_class,  no = positive_class)


  privileged[[positive_col]] = ifelse(test = privileged[[positive_col]] <= theta,
                                    yes = 1-privileged[[positive_col]], no = privileged[[positive_col]])


  privileged[negative_col] = ifelse(test = privileged[positive_col] <= theta,
                                    yes = 1-privileged[[negative_col]], no = privileged[[negative_col]])

  #### get rownames of deprived group

  lines_deprived = rownames(pred_mod[(pred_mod[[sensible_col]] != privileged_group) &
                                         (pred_mod[[classification_col]] == negative_class),])

  #### creating a dataframe only with deprived people

  deprived = pred_mod[rownames(pred_mod) %in% lines_deprived, ]

  ### if a deprived pearson gets a negative class with probability <= theta, then her/his classification turns positive

  deprived[[classification_col]] = ifelse(test = deprived[[negative_col]] <= theta,
                                        yes = positive_class, no = negative_class)

  deprived[[positive_col]] = ifelse(test = deprived[[negative_col]] <= theta,
                                  yes = 1-deprived[[positive_col]], no = deprived[[positive_col]])

  deprived[[negative_col]] = ifelse(test = deprived[[negative_col]] <= theta,
                                  yes = 1-deprived[[negative_col]], no = deprived[[negative_col]])


  ###########################

  ### removing lines selected in "privileged" and "deprived" dataframes

  remove = c(lines_privileged, lines_deprived)

  pred_mod = pred_mod[!(rownames(pred_mod) %in% remove),]

  #### adding updated lines from "privileged" and "deprived" dataframes

  pred_mod = rbind(pred_mod, privileged)
  pred_mod = rbind(pred_mod, deprived)


  return(pred_mod)

}

