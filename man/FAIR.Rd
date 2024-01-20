\title{
  FAIRification d'un jeu de données
}
\name{FAIR}
\alias{FAIR}
\description{
  Applique un traitement FAIR (findability, accessibility, interoperability, and reusability) sur le jeu de données.
  Retourne des metadonnées liées aux données, crée un codebook et sauvegarde la sortie dans un fichier .Rdata.
}\usage{
  FAIR(data)
}\arguments{
  \item{data}{
    Le jeu de données.
}
  \item{id}{
    Colonne d'identifiants si présente dans le jeu de données.
}
  \item{save.Rdata}{
    Choix de sauvegarder le jeu de données et ses métadonnées dans un fichier .Rdata. Par défaut, \code{save.Rdata = FALSE}.
}
  \item{print.metadata}{
    Choix d'ouvrir ou non la description de la sortie dans un nouvel onglet à l'éxécution. Par défaut, \code{print.metadata = TRUE}.
}
}\value{
  Une liste de metadonnées.
}\author{
  Hugo Marthinet
}\examples{
  FAIR(iris)
}
