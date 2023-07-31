###################################################################
#                                                                 #
#   30 de julho de 2023                                           #
#   FSL0540 Tópicos Avançados em Sociologia da Educação           #
#   Autor: Carolina Bueno Stefani e Guilherme Olímpio Fagundes    #
#   Script adaptado de: Fernanda Peres (Unifesp),                 #
#                       Vitor Alcantara (USP)                     #
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


# PASSO 2. Manusear banco de dados --------------------------------------------

# Como fatores
dados <- read.csv2("C:\\Users\\guilh\\Documents\\FSL0540_Final\\FSL0540_SAEB21_reg.csv",
                   stringsAsFactor=F)

glimpse(dados)

dados <- mutate(.data = dados,
                faixasNSE = case_when(
                  NSEAluno < 4.5 ~ "Baixo",
                  NSEAluno >= 4.5 & NSEAluno < 5.5 ~ "Regular",
                  NSEAluno >= 5.5 ~ "Alto"))

dados <-strings2factors(dados)
glimpse(dados)

# PASSO 3.Análise das frequências das categorias da variável dependente --------

summary(dados)

# PASSO 4. Checagem das categorias de referência -------------------------------

levels(dados$trabalha)  # Estudar = categoria de referência
levels(dados$raça)      # Branca = categoria de referência
levels(dados$mae)       # Fund- = categoria de referência
dados$mae <- relevel(dados$mae, ref="Superior+")
levels(dados$major)     # Privada = categoria de referência
dados$major <- relevel(dados$major, ref="Publica") # Refaço!
levels(dados$major)     # Publica = categoria de referência
levels(dados$faixasNSE) # Alta =categoria de referência

# PASSO 5. Checagem dos pressupostos -------------------------------------------

# A. Variável dependente nominal: SIM
# B. Independência das observações (sem medidas repetidas): SIM
# C. Ausência de multicolinearidade: SIM

psych::pairs.panels(dados[3:7])

# Testar um modelo de regressão linear para testar multicolinearidade
m <- lm(as.numeric(trabalha) ~ major + mae + raça + faixasNSE, data=dados)
car::vif(m) # Se algum valor é 10, tem multicol.


# D. Independência de alternativas irrelevantes (teste Hausman-McFadden):
install.packages("mlogit")
library(mlogit)

dadomnl <- mlogit::mlogit.data(dados, shape = "wide", choice="trabalha")

modiia <- mlogit::mlogit(trabalha ~ 1 | major + mae + raça + FaixasNSE,
                         data=dados, shape="wide",
                         reflevel = "Estudar")

modiia2 <- mlogit::mlogit(trabalha ~ 1 | major + mae + raça + FaixasNSE,
                         data=dados, shape="wide",
                         reflevel = "Estudar",
                         alt.subset = c("Estudar", "Trabalhar"))

modiia3 <- mlogit::mlogit(trabalha ~ 1 | major + mae + raça + FaixasNSE,
                          data=dados, shape="wide",
                          reflevel = "Estudar",
                          alt.subset = c("Estudar", "Nao_Sabe"))

mlogit::hmftest(modiia, modiia2)
mlogit::hmftest(modiia, modiia3)

# PASSO 6. Construção do modelo e interpretação dos resultados -----------------

# Construção domodelo e modelo nulo (usando pacote nnet)
mod <- multinom(trabalha ~ mae + raça + faixasNSE, data=dados, model=TRUE)
mod0 <- multinom(trabalha ~ 1, data=dados, model=TRUE) # Previsor


# Ajuste de modelo 
anova(mod, mod0)  # p<0.05 O modelo nulo é, logo, diferente do alternativo! Bom.
DescTools::PseudoR2(mod, which = "Nagelkerke") 

# Efeitos gerais
car::Anova(mod, type = "II", test="Wald")
# Se o Pr(>Chisq) for maior que 0.5, não é bom preditor... mas todos aqui são!

# Efeitos específicos
summary(mod)
 

# Obtenção dos valores de p por Wald (lmtest)
lmtest::coeftest(mod)
# 1) Coeficiente!
# 2) Ignoramos intercepts
# 3) Todos são previsores signitivativas para Estudar, Não_Sabe e Trabalhar
# 4) Positivo: A mãe com Superior completo, quando comparados com Fund-, o que
# significa que estudantes com m_S+ tem mais chances de não saber do que com 
# mães Fund-

## Obtenção das razões de chance com IC 95% (usando log-likelihood)
exp(coef(mod)) 
# As chances de assinalar Trabalhar é 2 vezes maior para uma pessoa preta do que
# para uma pessoa branca 


exp(confint(mod)) #IC
# Graças ao IC, tem que ser diferente a 1 (maeFund+, maeMédio+)

## Tabela completa (pacote gtsummary)
gtsummary::tbl_regression(mod, exponentiate = FALSE)
gtsummary::tbl_regression(mod, exponentiate = TRUE)


# PASSO 7. Segundo modelo ------------------------------------------------------

mod2 <- multinom(trabalha ~ raça + faixasNSE, data=dados, model=T) # Exclui escM

anova(mod0, mod2) # Ajuste do modelo 
DescTools::PseudoR2(mod2, which = "Nagelkerke")
Anova(mod2, type="II", test="Wald") # Efeitos gerais
summary(mod2)
lmtest::coeftest(mod2)
exp(coef(mod2))
exp(confint(mod2)) #IC


# PASSO 8. Comparando modelos

# AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)

# Qui-quadrado
anova(mod2, mod, test = "Chisq")

# PASSO 9. Tabelas e gráficos --------------------------------------------------

# Pelo modelo 2
tab <- table(Observado =dados$trabalha, Previsto = predict(mod2))
prop.table(tab)*100

acuracia = sum(diag(tab)) / sum(tab)
acuracia

caret::confusionMatrix(predict(mod2), dados$trabalha)
caret::confusionMatrix(predict(mod), dados$trabalha)

# Gráficos
dados_prev <- cbind(dados[3:7], predict(mod, type = "probs", se = TRUE))

dados_prev <- reshape2::melt(dados_prev,
                             id.vars = c("mae", "raça", "NSEAluno", "major", 
                                         "faixasNSE"),
                             value.name = "Probabilidade",
                             variable.name = "Escolha")

ggplot(dados_prev, aes(x = as.integer(NSEAluno), y = Probabilidade, color = ..x..)) +
  geom_smooth(method = "loess", size = 0.5) +
  labs(x = "Escore NSE") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    accuracy = 1),
                     breaks = seq(1, 10, 1)) +
  facet_grid(Escolha ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))
