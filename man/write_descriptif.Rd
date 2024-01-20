\title{
  Impression xlsx de descriptifs
}
\name{write_descriptif}
\alias{write.descriptif}
\description{
  Ecriture en format .xlsx, avec mise en page adaptée, d'un objet de type desc.univar ou desc.bivar.
}\usage{
  write.descritif(desc,
             file,
             columnsBorders = TRUE,
             boldLabels = TRUE,
             backgroundColors = FALSE,
             autoWidth = TRUE)
}\arguments{
  \item{desc}{
    Un tableau descriptif (ou une liste de tableaux) au format de sortie des fonctions \code{desc.univar} ou \code{desc.bivar}.
}
  \item{file}{
    Le chemin d'écriture en sortie. L'estension du fichier doit être \code{.xlsx} afin que la mise en page soit pris en compte.
}
  \item{columnsBorders}{
    Bordures de tableau. Par défaut \code{columnsBorders = TRUE} et les bordures sont affichées.
}
  \item{boldLabels}{
    Noms de variables en gras. Par défaut \code{boldLabels = TRUE}.
}
  \item{backgroundColors}{
    Séparation des varibles en nuances de gris. Par défaut \code{backgroundColors = FALSE}.
}
  \item{autoWidth}{
    Ajustement des largeur de colonnes. Par défaut \code{autoWidth = TRUE}.
}
}\value{
  Un fichier .xlsx mis en page.
}\author{
  Hugo Marthinet
}
\seealso{
  \code{\link{descriptif}}
}\examples{

}
