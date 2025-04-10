
library(sampling)
library(survey)

# leitura dos dados

data <- read_ods("amostra_email.ods")


# variáveis

colnames(data5) <-
  c("data_hora","NO_ENTIDADE","TP_DEPENDENCIA","QT_DOC_INF","QT_DOC_FUN
D","QT_DOC_MED","QT_TUR_INF","QT_TUR_FUND","QT_TUR_MED","IN_BIBLIO
TECA","IN_LABORATORIO_INFORMATICA","IN_INTERNET","IN_COZINHA","IN_P
ISCINA","IN_AUDITORIO")


# organização da amostra pelos estratos

data2 <- data2[order(data2$TP_DEPENDENCIA), ]


# estratificação teórica para o cálculo da probabilidade da amostra

IAESs=sampling::strata(zonamata2, stratanames=c("TP_DEPENDENCIA"),
                       c(9,4,2),method=c("srswor"))


# correção para população finita

fpc_email=rep(c(197,108,23),c(9,4,2))


# especificando o estilo de amostragem utilizado

Plano=svydesign(~1, strata=~TP_DEPENDENCIA, data = data2, probs=~IAESs$Prob,
                fpc=~fpc_email)


# médias amostrais e erros padrão das médias amostrais

svymean(~QT_DOC_INF,Plano)
svymean(~QT_DOC_FUND,Plano)
svymean(~QT_DOC_MED,Plano)
svymean(~QT_TUR_INF,Plano)
svymean(~QT_TUR_FUND,Plano)
svymean(~QT_TUR_MED,Plano)
svymean(~IN_BIBLIOTECA,Plano)
svymean(~IN_LABORATORIO_INFORMATICA,Plano)
svymean(~IN_INTERNET,Plano)
svymean(~IN_COZINHA,Plano)
svymean(~IN_PISCINA,Plano)
svymean(~IN_AUDITORIO,Plano)


# erro padrão das proporções amostrais para variáveis categóricas

# IN_BIBLIOTECA
sqrt((((197/328)^2)*((1*0)/9))+(((108/328)^2)*((1*0)/4))+(((23/328)^2)*((1*0)/2)))

# IN_LABORATORIO_INFORMATICA
sqrt((((197/328)^2)*((1*0)/9))+(((108/328)^2)*((0.75*0.25)/4))+(((23/328)^2)*((1*0)/2)))

# IN_INTERNET
sqrt((((197/328)^2)*((1*0)/9))+(((108/328)^2)*((1*0)/4))+(((23/328)^2)*((1*0)/2)))

# IN_COZINHA
sqrt((((197/328)^2)*((1*0)/9))+(((108/328)^2)*((1*0)/4))+(((23/328)^2)*((1*0)/2)))

# IN_PISCINA
sqrt((((197/328)^2)*(((1/9)*(8/9))/9))+(((108/328)^2)*((0*1)/4))+(((23/328)^2)*((0*1)/2)))

# IN_AUDITORIO
sqrt((((197/328)^2)*(((3/9)*(6/9))/9))+(((108/328)^2)*((0.75*0.25)/4))+(((23/328)^2)*((0*1)/2)))

