###libraries####################################################################
librarian::shelf(metaDigitise)

###Création du dossier##########################################################
#dir.create("data/articles_corpus/Cardinael_2019")

###Extraction des données quanti################################################
data <- metaDigitise(dir = "data/articles_corpus/Cardinael_2019")

