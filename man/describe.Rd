\title{
  describe
}
\name{describe}
\alias{describe}
\description{
  Compile un descriptif des variables d'un jeu de donnees. Le descriptif peut etre univarie ou bivarie (selon une variable stratifiante).
}\usage{
  describe(# Arguments de base
            data,
            vars,
            factor,
            label,
            weights,

            # Intervalles de confiance
            conf.level = 0.95,
            conf.method.cat = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson", "boot"),
            conf.method.num = c("classic", "boot"),

            # Variables quanti
            prop.type = c("col.percent", "row.percent", "tot.percent"),
            prop.test = c("test", "chi", "fisher"),
            chi.correct = FALSE,
            simulate.p.value = FALSE,

            # Variables quali
            num.type = c("mean", "both", "med"),
            mean.test = c("test", "student", "kruskal"),

            # Valeurs manquantes
            na.omit = TRUE,
            na.str.default = c("missing", "mode", "value"),
            na.str.value = NULL,
            na.num.default = c("none", "mean", "med", "value"),
            na.num.value = NULL,

            # Taux d'incidence
            pop.ref = NULL,

            # Décimales
            decimal = 1,
            cut.pvalue = 0.001,

            # P-valeur
            p.decimal = 3,
            p.adjust = FALSE,
            p.adjust.method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),

            # Colonnes à inclure
            include.n = TRUE,
            include.tot = TRUE,
            include.conf = TRUE,
            include.minmax = FALSE,
            include.p = TRUE,
            include.test.name = FALSE,

            # Mise en page
            lang = c("fr", "en"),
            merge.cols = TRUE)
}\arguments{
  \item{data}{
    Jeu de données sous forme de data frame.
}
  \item{vars}{
    Noms ou indices des colonnes de \code{data} a decrire.
    Par défaut toutes les colonnes de \code{data} sont decritent.
}
  \item{factor}{
    Nom ou indice de la variable stratifiante. Si \code{factor = NULL} (par defaut), le descriptif sera univarie, sinon, la valeur doit etre unique.
}
  \item{label}{
    Vecteur de chaînes de caractères qui servira de subsitut de libelles aux variables decrites,
    doit-être de même longueur que \code{vars}. Par défaut, les labels sont les noms de colonnes.
}
  \item{weights}{
    Nom ou indice de la colonne renseignant les poids des individus pour un calcul de moyenne ponderee. Si \code{weights = NULL} (par defaut), aucune pondération ne sera appliquee, sinon, la valeur doit etre unique.
}
  \item{conf.level}{
    La valeur du quantile de l'intervalle. Par défaut \code{conf.level = 0.95}.
    Doit être compris entre 0 et 1.
}
  \item{conf.method.cat}{
    La méthode de calcul de l'intervalle de confiance des proportions.
    Par défaut \code{conf.method = "waldcc"}.
    Les autres méthodes sont : \code{"cplus1"}, \code{"goodman"}, \code{"wald"}, \code{"sisonglaz"} et \code{"wilson"}, \code{boot}.
}
  \item{conf.method.num}{
    La méthode de calcul de l'intervalle de confiance des moyennes.
    Par défaut \code{conf.method = "classic"}.
    L'autres méthodesont : \code{"boot"} calcule l'IC par bootstrap
}
  \item{prop.type}{
    Type de pourcentage, en colonne (\code{prop.type = "col.percent"}) en ligne (\code{prop.type = "row.percent"}) ou sur le total
    (\code{prop.type = "tot.percent"}). Par defaut, les pourcentages sont calcules en colonne.
}
  \item{prop.test}{
    Type de test pour les fréquences. Par défaut \code{prop.test = "test"}, effectue d'abord le test du Chi2,
    puis celui de Fisher si le Chi2 ne convient pas, puis simule le \code{p-value} du test de Fisher si
    celui-ci ne convient pas. Les autres options sont \code{prop.test = "chi"} et \code{prop.test = "fisher"}.
}
  \item{chi.correct}{
    Applique la correction de continuité au test du Chi2. Par défaut \code{chi.correct = FALSE}.
}
  \item{simulate.p.value}{
    Choix de simuler la p-valeur du test de fisher par la méthode de Monte Carlo.
}
  \item{num.type}{
    Choix de compiler la moyenne (\code{quanti = "mean"}), la médiane (\code{quanti = "med"}) ou les deux (\code{quanti = "both"}) dans le cas d'une variable numerique.
    Par défaut, seule la moyenne est compilee (\code{quanti = "moy"}).
}
  \item{mean.test}{
    Type de test pour les moyennes. Par défaut \code{mean.test = "test"},
    afin de choisir entre le test de Student ou de Kruskal-Wallis selon la vérification d'egalite de variance.
    Les autres choix sont \code{mean.test = "student"} et \code{mean.test = "kruskal"} afin de forcer un test.
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
