#' @name adult.data
#' @title Adult Dataset
#' @description Sample extracted from the 1994 USA census. Each observation is related to an individual.
#' @format  Data frame with 32,561 rows and 15 columns.
#' \itemize{
#'   \item income: factor, >50K / <=50K. Income of each person.
#'   \item age: integer, age of each person.
#'   \item workclass factor, category of work. Private, Self-emp-not-inc, Self-emp-inc,
#'   Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#'   \item fnlwgt: numeric
#'   \item education: factor. Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc,
#'    9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#'   \item education_num: numeric. Variable education converted to numeric.
#'   \item marital-status:  factor of marital status. Married-civ-spouse, Divorced, Never-married, Separated, Widowed,
#'    Married-spouse-absent, Married-AF-spouse.
#'   \item occupation: factor. Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving,
#'    Priv-house-serv, Protective-serv, Armed-Forces.
#'   \item relationship:: factor. Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#'   \item race: factor. White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#'   \item capital_gain: numeric.
#'   \item capital_loss. numeric.
#'   \item hours-per-week: numeric. Worked hours per week.
#'   \item native-country:: factor. Country of origin.
#' }

#'
#' @examples  data('adult.data')
#'
#' @return Returns a data frame with 32,561 rows and 15 columns.
#'
#' @references

#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

#' @source   Dataset extracted from: \href{https://archive.ics.uci.edu/ml/datasets/adult}{UCL Machine Learning Repository}
NULL
