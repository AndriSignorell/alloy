
#' @encoding UTF-8
NULL


#' Los Angeles High School Attendance and Test Scores Data
#'
#' A dataset containing attendance records, demographic information, and 
#' standardized test scores for 316 high school students from two schools 
#' in Los Angeles. This dataset is frequently used to demonstrate count data 
#' models like Poisson and Negative Binomial regression.
#'
#' @docType data
#' @keywords datasets
#' @name Lahigh
#' @usage data("Lahigh")
#'
#' @format A data frame with 316 rows and 10 variables:
#' \describe{
#'   \item{id}{Student identification number.}
#'   \item{gender}{Student's gender (factor/categorical variable).}
#'   \item{ethnic}{Student's ethnicity (categorical variable, e.g., Hispanic, White, Black, Asian).}
#'   \item{school}{School attended by the student (Type 1 or Type 2).}
#'   \item{mathpr}{Math percentile rank from the Comprehensive Tests of Basic Skills (CTBS).}
#'   \item{langpr}{Language arts percentile rank from the Comprehensive Tests of Basic Skills (CTBS).}
#'   \item{mathnce}{Math Normal Curve Equivalent (NCE) score.}
#'   \item{langnce}{Language arts Normal Curve Equivalent (NCE) score.}
#'   \item{biling}{Bilingual status or English proficiency level (e.g., LEP for Limited English Proficiency).}
#'   \item{daysabs}{Number of days absent during the school year (count response).}
#' }
#' @source \url{https://stats.idre.ucla.edu/stat/data/lahigh.csv}
#' @source \url{https://stats.oarc.ucla.edu/stata/output/poisson-regression/}
"Lahigh"


#' Graduate school admissions
#'
#' Admissions decisions for 400 applicants to a graduate programme, used
#' to illustrate binary logistic regression.
#'
#' @format A data frame with 400 rows and 4 variables:
#' \describe{
#'   \item{admit}{Binary outcome: 1 = admitted, 0 = not admitted.}
#'   \item{gre}{GRE score.}
#'   \item{gpa}{Grade point average.}
#'   \item{rank}{Factor (1--4): prestige of undergraduate institution.}
#' }
#' @source \url{https://stats.oarc.ucla.edu/r/dae/logit-regression/}
"Admit"


#' Academic aptitude scores (tobit example)
#'
#' Scores on an academic aptitude test for 200 high school students,
#' right-censored at 800.  Used to illustrate tobit regression.
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{apt}{Academic aptitude score (censored at 800).}
#'   \item{id}{ID for the student}
#'   \item{read}{Reading score.}
#'   \item{math}{Math score.}
#'   \item{prog}{Factor: academic programme (\code{"academic"},
#'     \code{"general"}, \code{"vocational"}).}
#' }
#' @source \url{https://stats.oarc.ucla.edu/stata/dae/tobit-analysis/}
"Apt"


#' Ordinal outcome: applying to graduate school
#'
#' Survey data on 400 students' likelihood of applying to graduate school,
#' used to illustrate ordinal logistic regression.
#'
#' @format A data frame with 400 rows and 3 variables:
#' \describe{
#'   \item{apply}{Likelihood of applying to graduate school (ordered factor response: 0 = "unlikely", 1 = "somewhat likely", 2 = "very likely").}
#'   \item{pared}{Parental education indicator (1 = at least one parent has a graduate degree, 0 = otherwise).}
#'   \item{public}{Type of undergraduate institution (1 = public school, 0 = private school).}
#'   \item{gpa}{Student's cumulative undergraduate Grade Point Average (GPA).}
#' }
#' @source \url{https://stats.oarc.ucla.edu/r/faq/ologit-coefficients/}
#' @source \url{https://stats.idre.ucla.edu/stat/data/ologit.dta}
"Ologit"


#' Ice cream flavour preference
#'
#' Preference data for 200 respondents choosing between three ice cream
#' flavours, used to illustrate multinomial logistic regression and
#' machine learning classifiers.
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{id}{Respondent identifier.}
#'   \item{ice_cream}{Factor: preferred flavour (\code{"vanilla"} is
#'     reference, \code{"chocolate"}, \code{"strawberry"}).}
#'   \item{video}{Hours of video watched per week.}
#'   \item{puzzle}{Puzzle score.}
#'   \item{female}{Binary: 1 = female, 0 = male.}
#' }
#' @source \url{https://stats.oarc.ucla.edu/stata/output/multinomial-logistic-regression/}
"IceCream"


#' Worcester Heart Attack Study (WHAS100)
#'
#' Follow-up data for 100 patients hospitalised with acute myocardial
#' infarction, used to illustrate Cox proportional hazards and parametric
#' survival models.
#'
#' @format A data frame with 100 rows and 14 variables:
#' \describe{
#'   \item{id}{Subject identification number.}
#'   \item{addate}{Admission date to the hospital.}
#'   \item{foldate}{Follow-up date.}
#'   \item{hosstay}{Length of hospital stay (in days).}
#'   \item{foltime}{Follow-up time from hospital admission to follow-up date (in days).}
#'   \item{folstatus}{Vital status at follow-up (0 = alive, 1 = dead; event indicator).}
#'   \item{age}{Age at hospital admission (in years).}
#'   \item{gender}{Gender of the patient (0 = male, 1 = female).}
#'   \item{bmi}{Body Mass Index (kg/m\eqn{^2}).}
#'   \item{agex}{Age group (factor), derived from \code{age} via
#'    \code{\link[DescToolsX]{cutAge}}.}
#' }
#' @source \url{https://stats.oarc.ucla.edu/stata/examples/asa2/}
#' @references
#' Hosmer, D. W., Lemeshow, S., & May, S. (2008).
#' \emph{Applied Survival Analysis}, 2nd ed. Wiley.
"Whas100"


' Camper Fishing and Catch Data (Fish Dataset)
#'
#' A dataset containing information about 250 groups of visitors to a state park. 
#' This dataset is the standard textbook example for demonstrating Zero-Inflated 
#' Poisson (ZIP) and Zero-Inflated Negative Binomial (ZINB) regression models, 
#' as it contains an excess number of zeros in the count response variable.
#' 
#' @format A data frame with 250 rows and 6 variables:
#' \describe{
#'   \item{nofish}{Indicator for whether the group did not fish (1 = did not fish, 0 = fished).}
#'   \item{livebait}{Indicator for whether live bait was used (1 = yes, 0 = no).}
#'   \item{camper}{Indicator for whether the group brought a camper to the park (1 = yes, 0 = no).}
#'   \item{persons}{Total number of people in the group.}
#'   \item{child}{Number of children in the group.}
#'   \item{xb}{Linear predictor value for the count part of the model (generated simulation variable).}
#'   \item{zg}{Linear predictor value for the zero-inflation part of the model (generated simulation variable).}
#'   \item{count}{The number of fish caught (count response variable; target containing excess zeros).}
#' }
#' @source \url{https://stats.oarc.ucla.edu/r/dae/zip/}
"Fish"


#' Biochemists' publication counts
#'
#' Publication counts for 915 biochemistry PhD students, used to illustrate
#' zero-inflated and count regression models.  Originally from the
#' \pkg{pscl} package.
#'
#' @format A data frame with 915 rows and 6 variables:
#' \describe{
#'   \item{art}{Number of articles published in the last three years
#'     of the PhD (count response).}
#'   \item{fem}{Factor: gender (\code{"Men"}, \code{"Women"}).}
#'   \item{mar}{Factor: marital status (\code{"Single"}, \code{"Married"}).}
#'   \item{kid5}{Number of children under age 6.}
#'   \item{phd}{Prestige of PhD programme.}
#'   \item{ment}{Number of articles published by the mentor in the
#'     last three years.}
#' }
#' @source Long, J. S. (1997). \emph{Regression Models for Categorical and
#'   Limited Dependent Variables}. Sage.
"BioChemists"


#' Pima Indians diabetes data
#'
#' Clinical measurements for 768 Pima Indian women, used to illustrate
#' binary classification models.  Missing values (originally coded as 0)
#' have been replaced with \code{NA} for variables where 0 is physiologically
#' implausible.
#'
#' @format A data frame with 768 rows and 9 variables:
#' \describe{
#'   \item{pregnant}{Number of times pregnant.}
#'   \item{glucose}{Plasma glucose concentration (2-hour oral glucose
#'     tolerance test).}
#'   \item{pressure}{Diastolic blood pressure (mm Hg).}
#'   \item{triceps}{Triceps skin fold thickness (mm).}
#'   \item{insulin}{2-hour serum insulin (\eqn{\mu}U/ml).}
#'   \item{mass}{Body mass index.}
#'   \item{pedigree}{Diabetes pedigree function.}
#'   \item{age}{Age (years).}
#'   \item{diabetes}{Factor: \code{"neg"} (no diabetes) or \code{"pos"}
#'     (diabetes).}
#' }
#' @source \url{https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database}
#' @references
#' Smith, J. W., Everhart, J. E., Dickson, W. C., Knowler, W. C., &
#' Johannes, R. S. (1988). Using the ADAP learning algorithm to forecast
#' the onset of diabetes mellitus. \emph{Proceedings of the Annual Symposium
#' on Computer Application in Medical Care}, 261--265.
"Pima"



#' Contraceptive use in Bangladesh
#'
#' Data on contraceptive use by women in urban and rural areas of Bangladesh,
#' from the 1988 Bangladesh Fertility Survey.  Commonly used to illustrate
#' mixed-effects logistic regression with a district-level random effect.
#'
#' @format A data frame with 1934 rows and 6 variables:
#' \describe{
#'   \item{woman}{Factor: identifying code for each woman.}
#'   \item{district}{Factor: identifying code for each district (used as
#'     grouping variable in mixed models).}
#'   \item{use}{Factor: contraceptive use at time of survey
#'     (\code{"N"} = no, \code{"Y"} = yes).}
#'   \item{livch}{Ordered factor: number of living children at time of
#'     survey.  Levels are \code{0}, \code{1}, \code{2}, \code{3+}.}
#'   \item{age}{Numeric: age of woman at time of survey (years), centred
#'     around the mean.}
#'   \item{urban}{Factor: type of region of residence
#'     (\code{"urban"}, \code{"rural"}).}
#' }
#'
#' @examples
#' fitLogitMixed <- fitMod(use ~ age + urban + (1 | district),
#'                         data = Contraception, fitfn = "logitMixed")
#' fitLogitMixed
#' fitLogitMixed |> print(output = "or")
#'
#' @source \url{https://www.bristol.ac.uk/cmm/learning/mmsoftware/data-rev.html}
#' @references
#' Huq, N. M., and Cleland, J. (1990).
#' \emph{Bangladesh Fertility Survey 1989 (Main Report)}.
#' Dhaka: National Institute of Population Research and Training.
"Contraception"
