
library(readODS)

# leitura dos dados brutos do censo inep

censo23 <- read_ods("dados_censo.ods")


# filtragem dos dados do censo inep para a zona da mata

vinteteste <- censo23[censo23$NO_MESORREGIAO == "Zona da Mata", ]


# variáveis

colunas <- c("NO_ENTIDADE", "NO_MUNICIPIO", "CO_ENTIDADE", "NU_DDD",
             "NU_TELEFONE", "TP_DEPENDENCIA", "NO_MICRORREGIAO", "QT_DOC_INF",
             "QT_DOC_FUND", "QT_DOC_MED", "QT_TUR_INF" , "QT_TUR_FUND" ,
             "QT_TUR_MED" , "IN_BIBLIOTECA", "IN_LABORATORIO_INFORMATICA",
             "IN_INTERNET", "IN_COZINHA", "IN_PISCINA","IN_AUDITORIO")

vinteteste <- vinteteste[colunas]


# retirada das escolas de dependência federal

vintao <- subset(vinteteste, TP_DEPENDENCIA != 1)


# transformação das variáveis para melhor visualização

vintao$TP_DEPENDENCIA <- factor(vintao$TP_DEPENDENCIA, levels = c(2, 3, 4),labels
                                = c("Estadual", "Municipal","Privada"))

vintao$TP_DEPENDENCIA <- as.character(vintao$TP_DEPENDENCIA)


# retirada dos dados incompletos

doisfiltro <- vintao[complete.cases(vintao[, c("NO_ENTIDADE", "NO_MUNICIPIO",
                                               "CO_ENTIDADE", "NU_DDD", "NU_TELEFONE", "TP_DEPENDENCIA",
                                               "NO_MICRORREGIAO", "QT_DOC_INF" , "QT_DOC_FUND" , "QT_DOC_MED",
                                               "QT_TUR_INF" , "QT_TUR_FUND" , "QT_TUR_MED" , "IN_BIBLIOTECA",
                                               "IN_LABORATORIO_INFORMATICA", "IN_INTERNET", "IN_COZINHA",
                                               "IN_PISCINA","IN_AUDITORIO")]), ]


# leitura dos dados brutos secretaria da educação de minas

caminho2 <- "C:/Users/rafae/OneDrive/Desktop/Rafael/tcc/cadastro_lup.ods"

dadoslup <- read_ods(caminho2, sheet = 1, range = "A9:Q16774")


# padronização das células de CO_ENTIDADE para junção dos bancos de dado

filtro$CO_ENTIDADE <- substr(doisfiltro4$CO_ENTIDADE,
                             nchar(doisfiltro4$CO_ENTIDADE)-4, nchar(doisfiltro4$CO_ENTIDADE))

lup$Codigo_Escola <- substr(dadoslup$Codigo_Escola, nchar(dadoslup$Codigo_Escola)-4,
                            nchar(dadoslup$Codigo_Escola))


# fusão dos bancos de dado do censo inep da secretaria da educação de minas

df_merged <- merge(filtro, lup[, c("CO_ENTIDADE", "email", "Telefone_Fixo")], by =
                     "CO_ENTIDADE", all.x = TRUE)

df_merged2 <- df_merged[!is.na(df_merged$email), ]

df_merged2 <- df_merged2[, -ncol(df_merged2)]


# organização da população pelos estratos

zonamata2 <- zonamata[order(zonamata$TP_DEPENDENCIA), ]


# seleção das células sorteadas para a amostra após o sorteio

df_estadual <- df_merged2[df_merged2$TP_DEPENDENCIA == "Estadual", ]

df_municipal <- df_merged2[df_merged2$TP_DEPENDENCIA == "Municipal", ]

df_privada <- df_merged2[df_merged2$TP_DEPENDENCIA == "Privada", ]

df_privada_email <- df_privada[c(17, 12, 2, 7, 19, 1, 11, 6, 8, 9, 10, 18),]

df_privada_tel <- df_privada[-c(17, 12, 2, 7, 19, 1, 11, 6, 8, 9, 10, 18),]

df_estadual2 <- df_estadual[c(14, 193, 117, 11, 181, 146, 197, 23, 177, 30, 139, 16, 82, 118,
                              64, 128, 100, 179, 84, 40, 68, 9, 138, 107, 173, 44, 104, 180, 158, 143, 154, 114, 116, 137,
                              172, 65, 77, 61, 88, 157, 18, 97, 156, 194, 92, 108),]

df_estadual_email <- df_estadual2[c(1:23),]

df_estadual_tel <- df_estadual2[-c(1:23),]

df_municipal2 <- df_municipal[c(100, 68, 8, 76, 98, 30, 16, 19, 79, 64, 80, 83, 93, 35, 42, 94,
                                57, 12, 6, 62, 47, 9, 15, 92, 54, 48, 71, 73, 18, 81, 27, 4, 89, 108, 66, 84, 13, 3, 91, 107, 43,
                                75, 61, 29, 34),]

df_municipal_email <- df_municipal2[c(1:22),]

df_municipal_tel <- df_municipal2[-c(1:22),]

