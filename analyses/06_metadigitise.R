###libraries####################################################################
librarian::shelf(metaDigitise)

###Création du dossier##########################################################
#dir.create("data/articles_corpus/Cardinael_2019")

###Extraction des données quanti################################################
data <- metaDigitise(dir = "data/articles_corpus/s_8_Yin_2020")

#pour coller dans R
librarian::shelf(metaDigitise)
data <- metaDigitise(dir = "C:/Users/ASUS/Documents/GitHub/TRES-PRATIC/TRES-PRATIC/data/articles_corpus/s_24_Gallé_2019")
