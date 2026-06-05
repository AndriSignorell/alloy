


Rcpp::compileAttributes()

Rcpp::compileAttributes(getwd())
Rcpp::compileAttributes("C:/temp/alloy")

devtools::check()
devtools::install()

devtools::document()
devtools::load_all()

devtools::test()
devtools::run_examples()
devtools::install()


getwd()
f <- list.files(path = "C:/temp/alloy/R", full.names = T)

sapply(f, tools::showNonASCIIfile)


grep("car::", readLines("R/vif.R"), value = TRUE)
# und in anderen Files:
lapply(list.files("R", full.names = TRUE), function(f) {
  hits <- grep("car::|\\bcar\\b", readLines(f), value = TRUE)
  if (length(hits)) cat(f, "\n", hits, "\n")
})


fitLogit <- fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
r <- roc(fitLogit)
confint(r)

#=============================================================================
# prepare datasets


# The following code creates all datasets used in this vignette. Run once,
# then save with `usethis::use_data()`.

library(haven)
library(foreign)

# --- Lahigh: absenteeism in Los Angeles high schools ---
Lahigh <- removeAttr(read.dta("https://stats.idre.ucla.edu/stat/stata/notes/lahigh.dta"),
                     attrNames = names(attributes(Lahigh))[-c(3,11)]  )
usethis::use_data(Lahigh, overwrite = TRUE)


# --- Admit: graduate school admissions ---
Admit <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
Admit$rank <- factor(Admit$rank)
usethis::use_data(Admit, overwrite = TRUE)

# --- Apt: tobit regression example ---
Apt <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/stata/dae/tobit.dta"
) |> toBaseR()
usethis::use_data(Apt, overwrite = TRUE)

# --- Ologit: ordinal logistic regression ---
Ologit <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/data/ologit.dta"
) |> toBaseR()
usethis::use_data(Ologit, overwrite = TRUE)

# --- IceCream: multinomial logistic regression ---
IceCream <- haven::read_sas(
  "https://stats.idre.ucla.edu/wp-content/uploads/2016/02/mlogit.sas7bdat"
)
names(IceCream) <- tolower(names(IceCream))
IceCream$ice_cream <- relevel(
  factor(IceCream$ice_cream,
         labels = c("chocolate", "vanilla", "strawberry")),
  ref = "vanilla"
)
IceCream <- toBaseR(IceCream)
usethis::use_data(IceCream, overwrite = TRUE)

# --- Whas100: Worcester Heart Attack Study ---
Whas100 <- haven::read_dta(
  "https://stats.idre.ucla.edu/stat/examples/asa2/whas100.dta"
) |> toBaseR()
Whas100$addate <- as.Date(Whas100$addate, format = "%m/%d/%y")
Whas100$foldate <- as.Date(Whas100$foldate, format = "%m/%d/%y")
Whas100$agex <- cutAge(Whas100$age, full = FALSE)
usethis::use_data(Whas100, overwrite = TRUE)

# --- Fish: zero-inflated count data ---
Fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
Fish <- within(Fish, {
  nofish   <- factor(nofish)
  livebait <- factor(livebait)
  camper   <- factor(camper)
})
usethis::use_data(Fish, overwrite = TRUE)

# --- BioChemists: biochemists publication data (from pscl) ---
data("bioChemists", package = "pscl")
BioChemists <- bioChemists
usethis::use_data(BioChemists, overwrite = TRUE)



# pima diabetes
# alternative:
data(PimaIndiansDiabetes2, package = "mlbench")

# source: https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database?resource=download
Pima <- toBaseR(readr::read_delim("C:/temp/Pima.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE))
for( i in 2:8)
  Pima[Pima[,i] == 0, i] <- NA

colnames(Pima) <- c("pregnant","glucose","pressure","triceps",
                    "insulin","mass","pedigree","age","diabetes")

# data("Pima.tr2", package = "MASS")
# Pima <- Pima.tr2
usethis::use_data(Pima, overwrite = TRUE)

# for mixed models
data("Contraception", package = "mlmRev")
Contraception$age <- round(Contraception$age, 1)
usethis::use_data(Contraception, overwrite = TRUE)


# ===========================================================================


devtools::document()
devtools::load_all()

library(lme4)

fitLmm <- fitMod(Reaction ~ Days + (1|Subject), lme4::sleepstudy, fitfn = "lmMixed")
fitLmm

predict(fitLmm)


fitLogitMixed <- fitMod(cbind(incidence, size - incidence) ~ period + (1|herd),
                        data = lme4::cbpp, fitfn = "logitMixed")
fitLogitMixed
fitLogitMixed |> print(output = "or")

predict(fitLogitMixed)
predict(fitLogitMixed, output = "both")


fitPoisMixed <- fitMod(incidence ~ period + (1|herd),
                       data = lme4::cbpp, fitfn = "poissonMixed")
fitPoisMixed
fitPoisMixed |> print(output = "irr")

predict(fitPoisMixed)


# negbinMixed
fitNegbinMixed <- fitMod(incidence ~ period + (1|herd),
                         data = lme4::cbpp, fitfn = "negbinMixed")
fitNegbinMixed
predict(fitNegbinMixed)

# gammaMixed
fitGammaMixed <- fitMod(Reaction ~ Days + (1|Subject),
                        data = lme4::sleepstudy, fitfn = "gammaMixed")
fitGammaMixed

predict(fitGammaMixed)


# Klassischer binärer logitMixed
data("Contraception", package = "mlmRev")
fitLogitMixed <- fitMod(use ~ age + urban + (1|district),
                        data = Contraception, fitfn = "logitMixed")
fitLogitMixed
predict(fitLogitMixed, output = "both")






