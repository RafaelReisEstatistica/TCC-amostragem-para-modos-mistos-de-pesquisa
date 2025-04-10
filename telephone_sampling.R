
library(sampling)
library(survey)

# leitura dos dados

tel <- read_ods("amostra_tel.ods")


# variáveis

colnames(tel) <- c("data_hora", "NO_ENTIDADE", "TP_DEPENDENCIA",
                   "QT_DOC_INF", "QT_DOC_FUND", "QT_DOC_MED", "QT_TUR_INF",
                   "QT_TUR_FUND", "QT_TUR_MED")


# organização da amostra pelos estratos

tel <- tel[order(tel$TP_DEPENDENCIA), ]


# estratificação teórica para o cálculo da probabilidade da amostra para os dados

numéricos

IAESs=sampling::strata(zonamata2, stratanames=c("TP_DEPENDENCIA"), c(9,7,3),
                       method=c("srswor"))


# correção para população finita

fpc_tel=rep(c(197,108,23),c(9,7,3))


# especificando o estilo de amostragem utilizado

Plano=svydesign(~1, strata=~TP_DEPENDENCIA, data = tel, probs=~IAESs$Prob,
                fpc=~fpc_tel)


# médias amostrais e erros padrão das médias amostrais para os dados numéricos
svymean(~QT_DOC_INF,Plano)
svymean(~QT_DOC_FUND,Plano)
svymean(~QT_DOC_MED,Plano)
svymean(~QT_TUR_INF,Plano)
svymean(~QT_TUR_FUND,Plano)
svymean(~QT_TUR_MED,Plano)


# leitura dos dados categóricos

tel2 <- read_ods("amostra_tel2.ods")


# variáveis

colnames(tel2) <- c("data_hora", "NO_ENTIDADE", "TP_DEPENDENCIA",
                    "IN_BIBLIOTECA", "IN_LABORATORIO_INFORMATICA" ,"IN_INTERNET",
                    "IN_COZINHA", "IN_PISCINA", "IN_AUDITORIO")


# organização da amostra pelos estratos

tel2 <- tel2[order(tel2$TP_DEPENDENCIA), ]


# estratificação teórica para o cálculo da probabilidade da amostra para os dados

categóricos

IAESs=sampling::strata(zonamata2, stratanames=c("TP_DEPENDENCIA"),
                       c(11,7,5),method=c("srswor"))


# correção para população finita

fpc_tel2=rep(c(197,108,23),c(11,7,5))


# especificando o estilo de amostragem utilizado

Plano=svydesign(~1, strata=~TP_DEPENDENCIA, data = tel2, probs=~IAESs$Prob,
                fpc=~fpc_tel2)


# médias amostrais e erros padrão das médias amostrais para os dados numéricos

# IN_BIBLIOTECA
svymean(~IN_BIBLIOTECA,Plano)[2]
sqrt((((197/328)^2)*((1*0)/11))+(((108/328)^2)*((1*0)/7))+(((23/328)^2)*((0.8*0.2)/5)))

# IN_LABORATORIO_INFORMATICA
svymean(~IN_LABORATORIO_INFORMATICA,Plano)[2]
sqrt((((197/328)^2)*(((10/11)*(1/11))/11))+(((108/328)^2)*(((6/7)*(1/7))/7))+(((23/328)^2)*((0.6*0.4)/5)))

# IN_INTERNET
svymean(~IN_INTERNET,Plano)[2]
sqrt((((197/328)^2)*((1*0)/11))+(((108/328)^2)*((1*0)/7))+(((23/328)^2)*((1*0)/5)))

# IN_COZINHA
svymean(~IN_COZINHA,Plano)[2]
sqrt((((197/328)^2)*((1*0)/11))+(((108/328)^2)*((1*0)/7))+(((23/328)^2)*((0.4*0.6)/5)))

# IN_PISCINA
svymean(~IN_PISCINA,Plano)[2]
sqrt((((197/328)^2)*((0*1)/11))+(((108/328)^2)*((0*1)/7))+(((23/328)^2)*((0.4*0.6)/5)))

# IN_AUDITORIO
svymean(~IN_AUDITORIO,Plano)[2]
sqrt((((197/328)^2)*(((3/11)*(8/11))/11))+(((108/328)^2)*(((2/7)*(5/7))/7))+(((23/328)^2)*((0.6*0.4)/5)))

