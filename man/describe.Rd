\title{
  describe
}
\name{describe}
\alias{describe}
\description{
  Descriptive analysis of a dataset. Can be univariate of bivariate.
}\usage{
  describe(# Base arguments
            data,
            vars,
            factor,
            label,
            weights,

            # Confidence interval
            conf.level = 0.95,
            conf.method.cat = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson", "boot"),
            conf.method.num = c("classic", "boot"),

            # Quantitative variables
            prop.type = c("col.percent", "row.percent", "tot.percent"),
            prop.test = c("test", "chi", "fisher"),
            chi.correct = FALSE,
            simulate.p.value = FALSE,

            # Qualitative variables
            num.type = c("mean", "both", "med"),
            mean.test = c("test", "student", "kruskal"),

            # Missing values
            na.omit = TRUE,
            na.str.default = c("missing", "mode", "value"),
            na.str.value = NULL,
            na.num.default = c("none", "mean", "med", "value"),
            na.num.value = NULL,

            # Incidence rate
            pop.ref = NULL,

            # Decimal
            decimal = 1,
            cut.pvalue = 0.001,

            # p-value
            p.decimal = 3,
            p.adjust = FALSE,
            p.adjust.method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),

            # Columns to include
            include.n = TRUE,
            include.tot = TRUE,
            include.conf = TRUE,
            include.minmax = FALSE,
            include.p = TRUE,
            include.test.name = FALSE,

            # Formatting
            lang = c("fr", "en"),
            merge.cols = TRUE)
}\arguments{
  \item{data}{
    A data-frame containing the data. 
}
  \item{vars}{
Names or indexes of the columns of \code{data} to describe. By default, every columns are included.
}
  \item{factor}{
Name or index of the factor variable. Must be unique. If \code{factor = NULL} (default), only univariate analysis will be computed. 
}
  \item{label}{
Character vector of the names that will substitute the columns names in the output table. Must be the same length as \code{vars}.
By default, columns names are used.
}
  \item{weights}{
Name or index of the weight column for the weighted mean calculation. Must be unique. If \code{weights = NULL} (default), no weighting will be done
}
  \item{conf.level}{
Confidence interval level (default is 0.95). Must be between 0 and 1.
}
  \item{conf.method.cat}{
Method for the proportions confidence interval calculation. 
Default is \code{conf.method = "waldcc"}, other options are \code{"cplus1"}, \code{"goodman"}, \code{"wald"}, \code{"sisonglaz"} and \code{"wilson"}, \code{boot}.

}
  \item{conf.method.num}{
    Method for the numerical confidence interval calculation. 
    Default is \code{conf.method = "classic"}. Other option is \code{"boot"} for a bootstrap estimation.
}
  \item{prop.type}{
Type of the proportions to compile, which correspond to the side of the margins. 
Can either be \code{prop.type = "col.percent"} for a column total of a 100%, or bottom margin (default), 
\code{prop.type = "col.percent"} for a row total of a 100%, or right side margin, or \code{prop.type = "tot.percent"} for both
and a grand total of a 100%.
}
  \item{prop.test}{
Choice of which proportion test to use. Default method is \code{prop.test = "test"} which will first try to run 
a Chi-2 test and go onto a Fisher test if the Chi-2 does not converge. Other options are \code{prop.test = "chi"} and \code{prop.test = "fisher"}
to force either of of the two.
}
  \item{chi.correct}{
Enable the \code{correct} argument of the \code{chisq.test} function. Default is \code{chi.correct = TRUE} as in the original method.
}
  \item{simulate.p.value}{
Enable the \code{simulate.p.value} argument of the \code{fisher.test} function. Default is \code{simulate.p.value = FALSE} as in the original method.
}
  \item{num.type}{
Choice of which set of descriptive indicators to compute for the numerical variable. Can be either \code{quanti = "mean"}, \code{quanti = "med"} or \code{quanti = "both"}.
Standard deviation and mean CI95% are associated to the mean, IQR and meadian CI95% are associated to the median.
When choosing \code{quanti = "both"}, two lines are computed in the output datatable for each numerical variable.
}
  \item{mean.test}{
Choice of which mean test to use. Default method is \code{mean.test = "test"} which will first run a Bartlett test for homoscedasticity,
then apply either a Student or Kruskal-Wallis test depending on the outcome. 
Other options are \code{prop.test = "student"} and \code{prop.test = "kruskal"} to force either of of the two.
When the
    A noter quand dans le cas de l'inegalite de variances avec deux groupes, le test de Wilcoxson est substitué au Kruskal-Wallis.
}
  \item{na.omit}{
  Inclusion des donnees manquantes dans le descriptif. Par defaut \code{na.omit = TRUE} et les donnes manquantes sont ignorees.
}
  \item{na.str.default}{
    Choix de la methode d'imputation des valeurs manquantes pour les variables qualitatives (pris en compte uniquement si \code{na.omit = FALSE}). Les methodes possibles sont \code{"missing"} (par défaut) pour ajouter une modalité "Valeur manquante", \code{"mode"} pour imputer la modalite la plus frequente, et \code{"value"} pour imputer manuelement une valeur avec l'argument \code{"na.str.value"}.
}
  \item{na.str.value}{
    Choix de la valeur par defaut qui remplacera les valeurs manquantes pour les variables qualitatives quand \code{na.str.value = "value"}.
}
  \item{na.num.default}{
    Choix de la methode d'imputation des valeurs manquantes pour les variables quantitatives (pris en compte uniquement si \code{na.omit = FALSE}). Les methodes possibles sont \code{"none"} (par défaut), qui ne changera rien aux calculs, \code{"moy"} pour imputer la moyenne, \code{"med"} pour imputer la mediane, et \code{"value"} pour imputer manuelement une valeur avec l'argument \code{"na.num.value"}.
}
  \item{na.num.value}{
    Choix de la valeur par defaut qui remplacera les valeurs manquantes pour les variables quantitatives quand \code{na.num.value = "value"}
}
  \item{pop.ref}{
    Tableau de donnees contenant les effectifs de references pour calcul des taux d'incidence. La premiere colonne de \code{pop.ref} doit contenir le nom de la variable correpondante dans \code{data}, la deuxieme colonne doit correpondre aux modalites de chaques variable (toutes les modalites doivent etre presentes). Les colonnes suivantes doivent etre nommees selon les niveaux de la variable \code{factor} (inutile si \code{factor = NULL}) et contenir les effectifs dans la population de reference pour chaque modalite de chaque variable. \code{pop.ref}.
}
  \item{decimal}{
    Nombre de decimales des statistiques descriptives, par défaut 1. Les effectifs des tableaux de contingence ne présentent jamais de decimales.
}
  \item{cut.pvalue}{
    Point de coupure pour controler la valeur minimale affichee pour les p-values. Par défaut \code{cut.pvalue = 0.001} et une p-valeur inferieurs a 0.001 sera notee "<0.001".
}
  \item{p.decimal}{
    Nombre de décimales pour les p-valeurs, par défaut 3.
}
  \item{p.adjust}{
    Choix d'ajuster les p-valeurs des test par une methode de correction du alpha. Par defaut \code{p.adjust = FALSE}. (Cf. fonction \code{p.ajdust()} pour plus de detail)
}
  \item{p.adjust.method}{
    Choix de la methode de correction du alpha. Par defaut \code{p.adjust.method = "BH"}, les autres options possibles sont : "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr" et "none". (Cf. fonction \code{p.ajdust()} pour plus de detail)
}
  \item{include.n}{
    Inclusion de l'effectif de calcul. Par defaut \code{include.n = TRUE}.
}
  \item{include.tot}{
    Inclusion du descriptif sur le total (sans strate). Par défaut \code{include.tot = TRUE}.
}
  \item{include.conf}{
    Inclusion des intervalles de confiance. Par défaut \code{include.conf = TRUE}.
}
  \item{include.minmax}{
    Inclusion minimums et maximums pour les variables quantitatives et du nombre de modalites pour les variables qualitatives. Par défaut \code{include.range = FALSE}.
}
  \item{include.p}{
    Inclusion de la p-valeur du test. Par défaut \code{include.p = TRUE}.
}
  \item{include.test.name}{
    Inclusion du nom du test effectué. Par défaut \code{include.test.name = FALSE}.
}
  \item{lang}{
    Langue du descriptif. Par défaut \code{langue = "fr"},
    l'autre valeur possible est \code{langue = "en"}.
}
  \item{merge.cols}{
    Concatenation des colonnes de valeurs (effectifs/moyenne, pourcentage/ecart-type et IC). Par défaut \code{merge.cols = TRUE}.
}
}
\details{
  Les fonctions de calcul d'intervalles de confiance (les differents choix de \code{conf.method}) ne se basent pas sur des fonctions preexistantes dans R et les methodes sont directement implementees dans la fonction.

  L'algorithme qui determine le test a effectuer sur les moyennes (\code{mean.test = "test"}) se base sur le resultat du test de bartlett d'egalite des variances, et celui qui determine les test a effectuer sur les tableaux de contingences (\code{prop.test = "test"}) tente en premier lieu un test de Fisher avant d'effectuer un test de Chi-2 sur le test de Fisher n'aboutit pas. Les arguments \code{chi.correct} et \code{simulate.p.value} sont directement herites aux fonctions de test de Fisher et de Chi-2.

  Les methodes d'ajustement du alpha sont directement appliquees a la fonction \code{p.adjust}.
}
\value{
}
\author{
  Joffrey Marchi & Hugo Marthinet
}
\seealso{
  \code{\link{write.descriptif}}
}
\examples{
  # Exemple de configuration pour un descriptif univarié complet
  descriptif(data = iris,
           num.type = "both",
           decimal = 2,
           include.minmax = TRUE,
           merge.cols = FALSE)

  # Exemple de configuration minimale pour un descriptif bivarie
  descriptif(data = tab::tabdata,
           factor = "Group",
           vars = c("Age", "Sex", "Race", "BMI"))

  # Meme chose avec un accent sur les tests
  descriptif(data = tab::tabdata,
           factor = "Group",
           vars = c("Age", "Sex", "Race", "BMI"),
           label = c("Age", "Sexe", "Race", "IMC"),
           include.test.name = TRUE, include.tot = FALSE, include.n = FALSE,
           cut.pvalue = 0.0001,
           p.adjust = TRUE, p.adjust.method = "bonferroni")

  # Exemple d'utilisation de l'argument pop.ref pour un calcul de taux
  # Les colonnes 'male' et 'female' correspondent aux modalites de "gender" dans le jeu de donnees BEPS
  pop <- data.frame(
    Var = c(rep("vote", 3), rep("gender", 2)),
    Mod = c("Liberal Democrat", "Labour", "Conservative", "male", "female"),
    male = c(150000, 85000, 65000, 225000, 75000),
    female = c(10000, 5000, 6000, 22000, 7000)
  )
  descriptif(data = carData::BEPS,
             vars = c("vote", "gender", "age"),
             factor = "gender",
             pop.ref = pop)
}
