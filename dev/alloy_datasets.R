# ***************************************************************************
# dev/datasets.R (alloy) - where the data sets of the package come from
#
# Run once per data set, then document it in R/data.R. Taken unchanged from
# the former prepare.R, apart from Lahigh (see there).
# ***************************************************************************

stop("dev/datasets.R is a collection of commands: run them line by line, do not source the file.")

library(bedrock)      # toBaseR(), removeAttr()

# --- Lahigh: absenteeism in Los Angeles high schools ---
# two steps: the former one-liner referred to Lahigh inside its own definition
Lahigh <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/notes/lahigh.dta")
Lahigh <- removeAttr(Lahigh, attrNames = names(attributes(Lahigh))[-c(3, 11)])
usethis::use_data(Lahigh, overwrite = TRUE)

# --- Admit: graduate school admissions ---
Admit <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
Admit$rank <- factor(Admit$rank)
usethis::use_data(Admit, overwrite = TRUE)

# --- Apt: tobit regression example ---
Apt <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/dae/tobit.dta") |> toBaseR()
usethis::use_data(Apt, overwrite = TRUE)

# --- Ologit: ordinal logistic regression ---
Ologit <- haven::read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta") |> toBaseR()
usethis::use_data(Ologit, overwrite = TRUE)

# --- IceCream: multinomial logistic regression ---
IceCream <- haven::read_sas("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/mlogit.sas7bdat")
names(IceCream) <- tolower(names(IceCream))
IceCream$ice_cream <- relevel(
  factor(IceCream$ice_cream, labels = c("chocolate", "vanilla", "strawberry")),
  ref = "vanilla")
IceCream <- toBaseR(IceCream)
usethis::use_data(IceCream, overwrite = TRUE)

# --- Whas100: Worcester Heart Attack Study ---
Whas100 <- haven::read_dta("https://stats.idre.ucla.edu/stat/examples/asa2/whas100.dta") |> toBaseR()
Whas100$addate  <- as.Date(Whas100$addate,  format = "%m/%d/%y")
Whas100$foldate <- as.Date(Whas100$foldate, format = "%m/%d/%y")
Whas100$agex    <- DescToolsX::cutAge(Whas100$age, full = FALSE)
usethis::use_data(Whas100, overwrite = TRUE)

# --- Fish: zero-inflated count data ---
Fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
Fish <- within(Fish, {
  nofish   <- factor(nofish)
  livebait <- factor(livebait)
  camper   <- factor(camper)
})
usethis::use_data(Fish, overwrite = TRUE)

# --- BioChemists: publications of biochemists (from pscl) ---
data("bioChemists", package = "pscl")
BioChemists <- bioChemists
usethis::use_data(BioChemists, overwrite = TRUE)

# --- Pima: diabetes in Pima women ---
# source: https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database
# alternatives: data(PimaIndiansDiabetes2, package = "mlbench"); MASS::Pima.tr2
Pima <- toBaseR(readr::read_delim("C:/temp/Pima.csv", delim = ";",
                                  escape_double = FALSE, trim_ws = TRUE))
for (i in 2:8)
  Pima[Pima[, i] == 0, i] <- NA
colnames(Pima) <- c("pregnant", "glucose", "pressure", "triceps",
                    "insulin", "mass", "pedigree", "age", "diabetes")
usethis::use_data(Pima, overwrite = TRUE)

# --- Contraception: for the mixed models (from mlmRev) ---
data("Contraception", package = "mlmRev")
Contraception$age <- round(Contraception$age, 1)
usethis::use_data(Contraception, overwrite = TRUE)
