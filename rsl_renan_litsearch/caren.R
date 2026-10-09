
install.packages("remotes")
install.packages("devtools")
install.packages("revtools")
install.packages("stringi")
install.packages("stringr")
install_github("elizagrames/litsearchr", ref="main")
install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.10.tar.gz", repos = NULL, type="source")
install.packages("XML")
install.packages("bibtex")

library(remotes)
library(litsearchr)
library(devtools)
library(revtools)
library(stringi)
library(stringr)
library(bibtex)
library(dplyr)
library(XML)





#####Naive Search #####
#((Ciência OR  Pesquisa OR  Cientista OR Investigação OR Monitoramento OR 
# Conhecimento OR Mapeamento) 
# AND 
#(Cidadã OR Participativa OR Pública OR Colaborativa OR Ação OR Colaborativo OR 
#                                  Participativo OR Cidadão OR Público OR 
#                                 Comunitário OR “de base comunitária” OR 
#                                “Participação Pública”)
# AND (Brasil))

?import_results

#####Importing results#####

pasta <- setwd("G:/Meu Drive/Pesquisas/RBCC_Review/GT5_revisao/termos_busca_GT5")

#pasta <- setwd("C:/Users/caren/Desktop/GT5_RBCC/termos_busca") #pasta sumiu

scopus <- litsearchr :: import_results(pasta,file = "scopus_01.ris") #esta linha de comando não está dando erro.
scielo <- import_results(pasta, file = "scielo.ris")

lista_artigos <- bind_rows(scopus, scielo)

scopus