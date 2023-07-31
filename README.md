# FSL540 | Nota metodológica
Repositório com os bancos de dados e scripts de R.

## 1 Descrição dos procedimentos de coleta dos dados

Foram utilizados os dados das turmas das séries finais do Ensino Médio produzidos pelo Sistema Nacional de Avaliação da Educação Básica (SAEB), dos anos 2019 a 2021, para pensar a perspectiva profissional desses estudantes e as possíveis associações que tenham com suas origens sociais (cor ou raça, gênero, escolaridades de pais e mães e NSE). 

## 2 Descrição e justificação das técnicas mobilizadas no desenho de pesquisa

Mobilizamos duas técnicas de análise de dados estatísticos: (I) Análise de correspondência múltipla e (II) Regressão logíst. multinomial. Neste último, a perspectiva profissional desempenha o papel de *variável dependente* enquanto as demais assumem o papel de *variável independente*. Para a realização da pesquisa, duas técnicas estatísticas foram mobilizadas: (i) a análise de correspondência múltipla (doravante, ACM) e (ii) a regressão logística multinomial. Enquanto a primeira tem por objetivo explorar os dados que possuímos em mãos para identificar afinidades eletivas entre variáveis, a segunda – mais convencional aos estudos de estratificação horizontal – tem por objetivo explicar e predizer chances de que determinada modalidade (independente) possa influenciar outra (dependente).

Fonte dos bancos: https://www.gov.br/inep/pt-br/areas-de-atuacao/avaliacao-e-exames-educacionais/saeb/resultados

Para análise e sistematização, utilizamos a linguagem de programação R e seu LDE, RStudio, cujos scripts se encontram neste repositório; 

## IMPORTANTE
'FSL0540_RegressãoLogit.R' está o código da Regressão Logit. Multinomial

'FSL0540_ACM.R' está o código da ACM

'FSL0540_SAEB21_reg.csv' está o banco de dados utilizado para a Regressão Logit. Multinomial

'FSL0540_SAEB21_SP_ACM.csv' está o banco de dados utilizado para a ACM do SAEB 21 de SP

'FSL0540_SAEB19_SP_ACM.csv' está o banco de dados utilizado para a ACM do SAEB 19 de SP
