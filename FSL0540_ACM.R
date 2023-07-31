###################################################################
#                                                                 #
#   30 de julho de 2023                                           #
#   FSL0540 Tópicos Avançados em Sociologia da Educação           #
#   Autor: Carolina Bueno Stefani e Guilherme Olímpio Fagundes    #
#                                                                 #
###################################################################

# PASSO 1. Pacotes ------------------------------------------------

install.packages("pacman")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("misty")
install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Factoshiny")
install.packages("knitr")
install.packages("hrbrthemes")
install.packages("ggridges")
install.packages("forcats")

library(ggridges)
library(ggplot2)
library(knitr)
library(dplyr)
library(misty)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(pacman)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

pacman::p_load(rio, psych, descr, gridExtra, ggpubr, scales, 
               patchwork, effects, reshape2, foreign, sjPlot,
               pROC,ROCR,stargazer, caret,car, nnet, AER, 
               lmtest, gtsummary, DescTools)

# PASSO 2. Banco de dados para ACM --------------------------------

s21 <- read.csv("C:\\Users\\guilh\\Desktop\\SAEB2021_ALUNO.csv", header = T, sep = ";")
s19 <- read.csv("C:\\Users\\guilh\\Desktop\\SAEBs_2011-2019\\SAEB2019_ALUNO_EM.csv", header=T, sep=";")

# Selecionar apenas questionários respondidos E de SP

s21_sp_int <- dplyr::filter(s21, IN_PREENCHIMENTO_QUESTIONARIO %in% "1")
s19_sp_int <- dplyr::filter(s19, IN_PREENCHIMENTO_QUESTIONARIO %in% "1")

s21_sp_int <- dplyr::filter(s21_sp_int, ID_UF %in% "35")
s19_sp_int <- dplyr::filter(s19_sp_int, ID_UF %in% "35")

# Filtrar variáveis de interesse em cada SAEB

# SAEB 2021
s21_sp_int <- select(.data=s21_sp_int,
                     IN_PUBLICA, # Categoria administrativa
                     ID_SERIE,  
                     INSE_ALUNO, # NSE de Aluno (num)
                     NU_TIPO_NIVEL_INSE, # NSE de aluno (chr)
                     TX_RESP_Q01, # Sexo e gênero
                     TX_RESP_Q04, # Cor ou raça
                     TX_RESP_Q07, # Escolaridade da mãe
                     TX_RESP_Q08, # Escolaridade do pai
                     TX_RESP_Q17, # Você estudou majoritariamente...? 
                     TX_RESP_Q21) # O que você pretende...?

# SAEB 2019
s19_sp_int <- select(.data=s19_sp_int,
                     ID_SERIE,
                     TX_RESP_Q002, # Raça
                     TX_RESP_Q004, # Esc Mãe
                     TX_RESP_Q005, # Esc Pai
                     TX_RESP_Q014, # Você estudou majoritariamente...?
                     TX_RESP_Q019) # O que você pretende...?

# Renomeações de variáveis
s21_sp_int <- rename(.data = s21_sp_int,
                     "admin"=IN_PUBLICA,
                     "serie"=ID_SERIE,
                     "NSEAluno"=INSE_ALUNO,
                     "NSETipo"=NU_TIPO_NIVEL_INSE,
                     "sexo"=TX_RESP_Q01,
                     "raça"=TX_RESP_Q04,
                     "mae"=TX_RESP_Q07,
                     "pai"=TX_RESP_Q08,
                     "major"=TX_RESP_Q17,
                     "trabalha"=TX_RESP_Q21)

s19_sp_int <- rename(.data = s19_sp_int,
                     "serie"=ID_SERIE,
                     "raça"=TX_RESP_Q002,
                     "mae"=TX_RESP_Q004,
                     "pai"=TX_RESP_Q005,
                     "major"=TX_RESP_Q014,
                     "trabalha"=TX_RESP_Q019)

# Agora, filtramos os valores para excluir valores brancos e nulos

s21_sp_int <- dplyr::filter(s21_sp_int,
                            serie %in% c("12", "13"),
                            trabalha %in% c("A", "B", "C","D"),
                            pai %in% c("A", "B", "C", "D", "E", "F"),
                            mae %in% c("A", "B", "C", "D", "E", "F"),
                            raça %in% c("A", "B", "C", "D", "E"),
                            major %in% c("A", "B", "C"))

s19_sp_int <- dplyr::filter(s19_sp_int, 
                            serie %in% c("12", "13"),
                            trabalha %in% c("A", "B", "C","D"),
                            pai %in% c("A", "B", "C", "D", "E", "F"),
                            mae %in% c("A", "B", "C", "D", "E", "F"),
                            raça %in% c("A", "B", "C", "D", "E"),
                            major %in% c("A", "B", "C"))

# Recodificar variáveis, renomeando-as!

# SAEB21 Raça
s21_sp_int$raça <- dplyr::recode(s21_sp_int$raça,
                                 "A"="Branca",   # 46,8%
                                 "B"="Preta",    # 14%
                                 "C"="Parda",    # 36,5%
                                 "D"="Amarela",  # 1,9% 
                                 "E"="Indigena") # <1%
# SAEB21 Escolaridade da mãe
s21_sp_int$mae <- dplyr::recode(s21_sp_int$mae,
                                "A"="Fund-",  # 8,8%
                                "B"="Fund-",  # 10,7%
                                "C"="Fund-",  # 15,8%
                                "D"="Fund+",  # 39,3%
                                "E"="Medio+", # 13,5%
                                "F"="Superior+") # 11,8%

# SAEB21 Escolaridade do pai
s21_sp_int$pai <- dplyr::recode(s21_sp_int$pai,
                                "A"="Fund-",
                                "B"="Fund-",
                                "C"="Fund-",
                                "D"="Fund+",
                                "E"="Medio+",
                                "F"="Superior+")

# SAEB21 Escolha imediata
s21_sp_int$trabalha <- dplyr::recode(s21_sp_int$trabalha,
                                     "A"="Estudar",
                                     "B"="Trabalhar",
                                     "C"="Est+Tra",
                                     "D"="NS")

# SAEB21 Categoria administrativa estudada
s21_sp_int$major <- dplyr::recode(s21_sp_int$major, 
                                  "A"="Publica",
                                  "B"="Privada",
                                  "C"="Pub+Pri")

# SAEB21 NSE Categórica
s21_sp_int$NSETipo <- dplyr::recode(s21_sp_int$NSETipo,
                                    "1"="I",  # Reuni 1 e 2
                                    "2"="I",
                                    "3"="II",
                                    "4"="III",
                                    "5"="IV",
                                    "6"="V",
                                    "7"="VI", # Reuni 7 e 8 
                                    "8"="VI")

# SAEB19 Raça
s19_sp_int$raça <- dplyr::recode(s19_sp_int$raça,
                                 "A"="Branca",   # 44,7%
                                 "B"="Preta",    # 13,5%
                                 "C"="Parda",    # 38,5%
                                 "D"="Amarela",  # ~2,3% 
                                 "E"="Indigena") # ~1%

# SAEB19 Escolaridade da mãe
s19_sp_int$mae <- dplyr::recode(s19_sp_int$mae,
                                "A"="Fund-",  # 8,8%
                                "B"="Fund-",  # 10,7%
                                "C"="Fund-",  # 15,8%
                                "D"="Fund+",  # 39,3%
                                "E"="Medio+", # 13,5%
                                "F"="Superior+") # 11,8%

# SAEB19 Escolaridade do pai
s19_sp_int$pai <- dplyr::recode(s19_sp_int$pai,
                                "A"="Fund-",
                                "B"="Fund-",
                                "C"="Fund-",
                                "D"="Fund+",
                                "E"="Medio+",
                                "F"="Superior+")

# SAEB19 Escolha imediata
s19_sp_int$trabalha <- dplyr::recode(s19_sp_int$trabalha,
                                     "A"="Estudar",
                                     "B"="Trabalhar",
                                     "C"="Est+Tra",
                                     "D"="NS")

# SAEB19 Série
s19_sp_int$serie <- dplyr::recode(s19_sp_int$serie, 
                                  "12"="3EM",
                                  "13"="Int")

# SAEB19 Categoria administrativa estudada
s19_sp_int$major <- dplyr::recode(s19_sp_int$major, 
                                  "A"="Publica",
                                  "B"="Privada",
                                  "C"="Pub+Pri")

# Exportar para facilitar carregamentos
write.csv2(s21_sp_int, file="FSL0540_SAEB21_SP_ACM.csv")
write.csv2(s19_sp_int, file="FSL0540_SAEB19_SP_ACM.csv")

# De preferência, criar duplicatas dos bancos!
s21_sp_int_ACM <- select(.data= s21_sp_int,
                         raça, mae, pai, major, trabalha)
s21_sp_int_ACM_NSE <- select(.data= s21_sp_int,
                             raça, mae, pai, major, trabalha, NSETipo)
s19_sp_int_ACM <- select(.data = s19_sp_int,
                         raça, mae, pai, major, trabalha)


# PASSO 3. Análise de correspondência múltipla --------------------


# Primeira etapa: Visualização prévia
res.ACM_s21 <- MCA(s21_sp_int_ACM, graph = FALSE)
res.ACM_s21_NSE <- MCA(s21_sp_int_ACM_NSE, graph = FALSE)
res.ACM_s19 <- MCA(s19_sp_int_ACM, graph = FALSE)

# Segunda etapa: Análise dos resultados estatísticos
res.ACM_s21$eig      # Resultados dos eixos
res.ACM_s21$var$eta2 # Resultados das colunas
res.ACM_s21$var$cos2 # Resultados das variáveis
res.ACM_s21$var$v.test
res.ACM_s21$var$coord
res.ACM_s21$var$contrib

res.ACM_s21_NSE$eig      # Resultados dos eixos
res.ACM_s21_NSE$var$eta2 # Resultados das colunas
res.ACM_s21_NSE$var$cos2 # Resultados das variáveis
res.ACM_s21_NSE$var$v.test
res.ACM_s21_NSE$var$coord
res.ACM_s21_NSE$var$contrib

res.ACM_s19$eig      # Resultados dos eixos
res.ACM_s19$var$eta2 # Resultados das colunas
res.ACM_s19$var$cos2 # Resultados das variáveis
res.ACM_s19$var$v.test
res.ACM_s19$var$coord
res.ACM_s19$var$contrib

# Terceira etapa: Construção de tabelas

# Criando tabelas no R
tab1_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$eig))
tab2_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$var$eta2))
tab3_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$var$contrib))

tab1_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$eig))
tab2_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$var$eta2))
tab3_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$var$contrib))


tab1_s19 <- rownames_to_column(as.data.frame(res.ACM_s19$eig))
tab2_s19 <- rownames_to_column(as.data.frame(res.ACM_s19$var$eta2))
tab3_s19 <- rownames_to_column(as.data.frame(res.ACM_s19$var$contrib))

# Renomeando as colunas
names(tab1_s21)[1] <- "Dimensão"
names(tab1_s21)[2] <- "Eigenvalue"
names(tab1_s21)[3] <- "Variância (%)"
names(tab1_s21)[4] <- "Variância acumulada (%)"
names(tab2_s21)[1] <- "Colunas"
names(tab3_s21)[1] <- "Variáveis"

names(tab1_s21NSE)[1] <- "Dimensão"
names(tab1_s21NSE)[2] <- "Eigenvalue"
names(tab1_s21NSE)[3] <- "Variância (%)"
names(tab1_s21NSE)[4] <- "Variância acumulada (%)"
names(tab2_s21NSE)[1] <- "Colunas"
names(tab3_s21NSE)[1] <- "Variáveis"

names(tab1_s19)[1] <- "Dimensão"
names(tab1_s19)[2] <- "Eigenvalue"
names(tab1_s19)[3] <- "Variância (%)"
names(tab1_s19)[4] <- "Variância acumulada (%)"
names(tab2_s19)[1] <- "Colunas"
names(tab3_s19)[1] <- "Variáveis"

# Exportar!
write.xlsx(tab1_s21, file="FSL0540_SAEB21_Tab1.xlsx")
write.xlsx(tab2_s21, file="FSL0540_SAEB21_Tab2.xlsx")
write.xlsx(tab3_s21, file="FSL0540_SAEB21_Tab3.xlsx")

write.xlsx(tab1_s21NSE, file="FSL0540_SAEB21NSE_Tab1.xlsx")
write.xlsx(tab2_s21NSE, file="FSL0540_SAEB21NSE_Tab2.xlsx")
write.xlsx(tab3_s21NSE, file="FSL0540_SAEB21NSE_Tab3.xlsx")

write.xlsx(tab1_s19, file="FSL0540_SAEB19_Tab1.xlsx")
write.xlsx(tab2_s19, file="FSL0540_SAEB19_Tab2.xlsx")
write.xlsx(tab3_s19, file="FSL0540_SAEB19_Tab3.xlsx")

# Quarta etapa: Gráficos!

SAEB2021_ACM <- fviz_mca_var(res.ACM_s21,   #SAEB2021
                             col.var = "contrib", # Cor muda conforme contribuição da variáv.
                             repel=T, # Afasta os rótulos originais para + visual.
                             axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2021_ACM

SAEB2021_ACM_NSE <- fviz_mca_var(res.ACM_s21_NSE,   #SAEB2021
                                 col.var = "contrib", # Cor muda conforme contribuição da variáv.
                                 repel=T, # Afasta os rótulos originais para + visual.
                                 axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2021_ACM_NSE

SAEB2019_ACM <- fviz_mca_var(res.ACM_s19,   #SAEB2019
                             col.var = "contrib", # Cor muda conforme contribuição da variáv.
                             repel=T,       # Afasta os rótulos originais para + visual.
                             axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2019_ACM
