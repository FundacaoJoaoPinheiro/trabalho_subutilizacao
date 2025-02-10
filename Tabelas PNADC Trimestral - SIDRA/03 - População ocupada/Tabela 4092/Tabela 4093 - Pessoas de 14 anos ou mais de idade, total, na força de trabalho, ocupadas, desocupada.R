################################################################################
## LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG        ##
################################################################################

## Tabela 4093 - Pessoas de 14 anos ou mais de idade, total, na força de trabalho, ocupadas, desocupadas, fora da força de trabalho, em situação de informalidade e respectivas taxas e níveis, por sexo

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

library(PNADcIBGE)
library(survey)
library(tidyverse)

## Para que a função funcione, será necessário criar o caminho data/documentacao dentro do diretório principal
## Fazer isso uma única vez.
## Feitos os caminhos, mova o input para "caminhoprincipal/data/documentacao" e os dados para "caminhoprincipal/data/txt"
## Código para criar os caminhos:

# dir.create("data/documentacao", recursive = TRUE)
# input <- "data/documentacao/input_PNADC_anual.txt"
# print(file.exists(arquivo_dicionario)) 
# Se retornar TRUE, o código deu certo. O arquivo input está no lugar certo

# dir.create("D:/FJP2425/Programacao/data/txt", recursive = TRUE)
# dados <- "data/txt/PNADC_012012.txt"
# print(file.exists(dados))
# Se retornar TRUE, o código deu certo. O arquivo input está no lugar certo

# Código para criar o caminho que armazena as bases em .RDS:

#dir.create("C:/Users/italo/Desktop/MICRODADOS/estimativas", recursive = TRUE)


## Testando com as bases no diretório
## Foram feitos os downloads de todos os arquivos .txt da PNADC no diretório

## Função que automatiza a chamada dos dados
## Tudo uma única função

## Alterando a chamada dos estratos -> utilizando %in%
calcula_ocup_desocup <- function(ano){
  pnadc <- pnadc_design(read_pnadc(paste0("C:/Users/italo/Desktop/MICRODADOS/txt/PNADC_",ano,".txt"), 
                                   paste0("C:/Users/italo/Desktop/MICRODADOS/doc/input_PNADC_",ano,".txt"),vars=c("VD4001", "VD4002","V2007", "V2009", "V2010", "VD4009"))) %>% 
    update(
           #criação das variáveis da Tabela 4092
           piah = as.numeric(V2009 >= 14 & V2007==1),
           piam = as.numeric(V2009 >= 14 & V2007==2),
           
           fth = as.numeric(VD4001 == 1 & V2007==1),
           ftm = as.numeric(VD4001 == 1 & V2007==2),
           
           ocuph = 1 * (VD4002==1 & V2007==1),
           ocupm = 1 * (VD4002==1 & V2007==2),
           
           desocuph = 1 * (VD4002==2 & V2007==1),
           desocupm = 1 * (VD4002==2 & V2007==2),
           
           forafth = as.numeric(VD4001==2 & V2007==1),
           foraftm = as.numeric(VD4001==2 & V2007==2),
          
           
           d.homem = 1 * (V2007 == "Homem"),
           d.mulher = 1 * (V2007 == "Mulher"),
           
           raca_cor = case_when(
             V2010 == "Preta" | V2010 == "Parda" ~ "Preta ou parda",
             V2010 == "Amarela"  |
               V2010 == "Indígena" | V2010 == "Ignorado" ~ "Outras",
             V2010 == "Branca" ~ "Branca"
           ),
           
           faixa_idade = case_when((V2009 >= 14 &
                                      V2009 <= 29) ~ "0De14a29anos",
                                   (V2009 >= 30 &
                                      V2009 <= 44) ~ "de30a44anos",
                                   (V2009 >= 45 &
                                      V2009 <= 59) ~ "de45a59anos",
                                   (V2009 >= 60) ~ "de60a+anos"
           ),
           regioes = case_when(
             Estrato %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~"01-Belo Horizonte",
             Estrato %in% c("3120011","3120013","3120020","3120012") ~"02-Entorno metropolitono de BH",
             Estrato %in% c("3130011","3130012","3130020") ~"03-Colar metropolitano de BH",
             Estrato %in% c("3140010","3140020") ~"04-RIDE de Brasília em Minas",
             Estrato %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~"05-Sul de Minas",
             Estrato %in% c("3152011","3152012","3152013","3152021","3152022") ~"06-Triângulo Mineiro",
             Estrato %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~"07-Mata de Minas Gerais",
             Estrato %in% c("3154011","3154012","3154013","3154021","3154022","3154023") ~"08-Norte de Minas",
             Estrato %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~"09-Vale do Rio Doce",
             Estrato %in% c("3156011","3156012","3156013","3156021","3156022") ~"10-Central",
             TRUE ~ "11 - Minas Gerais")
           # regioes
    )
  
  # Criando estimativas regionais (reescrevendo parte do código)
  
  ## Total - População com mais de 14 anos, homens
  estimativas_1 <- svyby(~piah, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_1)[3]<-"cv1"
  
  ## Total - População com mais de 14 anos, mulheres
  estimativas_2 <- svyby(~piam, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_2)[3]<-"cv2"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens
  estimativas_3 <- svyby(~fth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_3)[3]<-"cv3"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres
  estimativas_4 <- svyby(~ftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_4)[3]<-"cv4"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens, ocupados
  estimativas_5 <- svyby(~ocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_5)[3]<-"cv5"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres, ocupadas
  estimativas_6 <- svyby(~ocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_6)[3]<-"cv6"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens, desocupados
  estimativas_7 <- svyby(~desocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_7)[3]<-"cv7"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres, desocupadas
  estimativas_8 <- svyby(~desocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_8)[3]<-"cv8"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens, desocupados
  estimativas_9 <- svyby(~forafth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_9)[3]<-"cv9"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres, desocupadas
  estimativas_10 <- svyby(~foraftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_10)[3]<-"cv10"
  
  #Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  #estimativas_11 <- svyby(numerator=~fth, denominator=~piah, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_11)[3]<-"cv11"
  
  #Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  #estimativas_12 <- svyby(numerator=~ftm, denominator=~piam, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_12)[3]<-"cv12"
  
  #Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  #estimativas_13 <- svyby(numerator=~desocuph, denominator=~fth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_13)[3]<-"cv13"
  
  #Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  #estimativas_14 <- svyby(numerator=~desocupm, denominator=~ftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_14)[3]<-"cv14"
  
  #Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  #estimativas_15 <- svyby(numerator=~informaish, denominator=~ocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_15)[3]<-"cv15"
  
  #Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  #estimativas_16 <- svyby(numerator=~informaism, denominator=~ocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  #colnames(estimativas_16)[3]<-"cv16"
  
  
  # Juntando as bases -> estratos
  estimativas <- estimativas_1 %>% 
    left_join(estimativas_2) %>% 
    left_join(estimativas_3) %>%
    left_join(estimativas_4) %>%
    left_join(estimativas_5) %>%
    left_join(estimativas_6) %>%
    left_join(estimativas_7) %>%
    left_join(estimativas_8) %>%
    left_join(estimativas_9) %>%
    left_join(estimativas_10) %>%
    #left_join(estimativas_11) %>%
    #left_join(estimativas_12) %>%
    #left_join(estimativas_13) %>%
    #left_join(estimativas_14) %>%
    #left_join(estimativas_15) %>%
    #left_join(estimativas_16) %>%
    mutate(periodo = paste0(ano)) %>%

  
  
  rm(estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, estimativas_12, estimativas_13, estimativas_14, estimativas_15, estimativas_16, pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas,paste0("C:/Users/italo/Desktop/MICRODADOS/estimativas/resultados_",ano,".RDS"))
  paste("Concluído:",ano)
  
}
## Fim da função

## Atualizando lista até 2023:
## Continuando a formatação por colunas

lista <- c(2012,2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

sapply(lista, function(i) calcula_ocup_desocup(i))

## Testando leitura de um arquivo

pnad2023<-readRDS("C:/Users/italo/Desktop/MICRODADOS/estimativas/resultados_2012.RDS")
view(pnad2023)

#Calcular os coeficientes de variação 
################################################################################
##          CRIANDO OS DFS PARA OS 10 ESTRATOS - 2012 A 2023                  ##
################################################################################


# Diretório principal
setwd("C:/Users/italo/Desktop/MICRODADOS/")

library(PNADcIBGE)
library(survey)
library(tidyverse)

## Lendo todos os arquivos .RDS
## Lembrete: o caminho deles está dentro do diretório principal
arquivos <- list.files("C:/Users/italo/Desktop/MICRODADOS/estimativas", pattern = "\\.RDS$", full.names = TRUE)
pnadcrds <- lapply(arquivos, readRDS)
str(pnadcrds) # para conferir o objeto

# Diagnóstico do upload:
dimensao <- sapply(pnadcrds, dim)
dimensao_df <- data.frame(t(dimensao))
colnames(dimensao_df) <- c("Linhas", "Colunas")
print(dimensao_df)
View(pnadcrds[[26]])  # Possível conferir os dados com o site do IBGE

################################################################################
## CRIANDO OS 11 DFS
## Minas Gerais (linha: 11)
# Estimativas diretas

mg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                     5,6,7,8,
                                     9,10,11,12)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[11, 10]),
                                         "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[11,2]),
                                         "cv" = as.numeric(df[11,3]),
                                         "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[11, 4]),
                                         "cv" = as.numeric(df[11,5]),
                                         "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[11, 6]),
                                         "cv" = as.numeric(df[11,7]),
                                         "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[11,8]),
                                         "cv" = as.numeric(df[11,9])
                                       )
                                     }))
#View(mg)

################################
### Belo Horizonte (linha: 1)

bh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                     5,6,7,8,
                                     9,10,11,12)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1, 10]),
                                         "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[1, 4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[1, 6]),
                                         "cv" = as.numeric(df[1,7]),
                                         "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[1,8]),
                                         "cv" = as.numeric(df[1,9])
                                       )
                                     }))

#View(bh)

######################################
### Entorno Metropolitano de BH (linha: 2)

entornobh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                            5,6,7,8,
                                            9,10,11,12)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2, 10]),
                                                "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[2,2]),
                                                "cv" = as.numeric(df[2,3]),
                                                "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[2, 4]),
                                                "cv" = as.numeric(df[2,5]),
                                                "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[2, 6]),
                                                "cv" = as.numeric(df[2,7]),
                                                "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[2,8]),
                                                "cv" = as.numeric(df[2,9])
                                              )
                                            }))

#View(entornobh)

########################################
### Colar Metropolitano de BH (linha: 3)

colarbh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 10]),
                                              "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[3,2]),
                                              "cv" = as.numeric(df[3,3]),
                                              "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[3, 4]),
                                              "cv" = as.numeric(df[3,5]),
                                              "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[3, 6]),
                                              "cv" = as.numeric(df[3,7]),
                                              "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[3,8]),
                                              "cv" = as.numeric(df[3,9])
                                            )
                                          }))

#View(colarbh)

########################################
### RIDE de Brasília em Minas (linha: 4)

RIDE<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 10]),
                                           "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[4, 4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[4, 6]),
                                           "cv" = as.numeric(df[4,7]),
                                           "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[4,8]),
                                           "cv" = as.numeric(df[4,9])
                                         )
                                       }))

#View(RIDE)

#####################################
### Sul de Minas (linha: 5)
sulmg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                        5,6,7,8,
                                        9,10,11,12)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[5, 10]),
                                            "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[5,2]),
                                            "cv" = as.numeric(df[5,3]),
                                            "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[5, 4]),
                                            "cv" = as.numeric(df[5,5]),
                                            "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[5, 6]),
                                            "cv" = as.numeric(df[5,7]),
                                            "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[5,8]),
                                            "cv" = as.numeric(df[5,9])
                                          )
                                        }))

#View(sulmg)

########################################################
### Triângulo Mineiro (linha: 6)

trng<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[6, 10]),
                                           "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[6,2]),
                                           "cv" = as.numeric(df[6,3]),
                                           "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[6, 4]),
                                           "cv" = as.numeric(df[6,5]),
                                           "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[6, 6]),
                                           "cv" = as.numeric(df[6,7]),
                                           "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[6,8]),
                                           "cv" = as.numeric(df[6,9])
                                         )
                                       }))

#View(trng)

###############################################################
### Mata de Minas Gerais (linha: 7)

zonamata<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                           5,6,7,8,
                                           9,10,11,12)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[7, 10]),
                                               "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[7,2]),
                                               "cv" = as.numeric(df[7,3]),
                                               "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[7, 4]),
                                               "cv" = as.numeric(df[7,5]),
                                               "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[7, 6]),
                                               "cv" = as.numeric(df[7,7]),
                                               "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[7,8]),
                                               "cv" = as.numeric(df[7,9])
                                             )
                                           }))

#View(zonamata)

############################################################
### Norte de Minas (Linha: 8)

nortemg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8, 10]),
                                              "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[8, 4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[8, 6]),
                                              "cv" = as.numeric(df[8,7]),
                                              "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[8,8]),
                                              "cv" = as.numeric(df[8,9])
                                            )
                                          }))

#View(nortemg)

######################################################
### Vale do Rio Doce (linha: 9)

riodoce<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[9, 10]),
                                              "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[9,2]),
                                              "cv" = as.numeric(df[9,3]),
                                              "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[9, 4]),
                                              "cv" = as.numeric(df[9,5]),
                                              "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[9, 6]),
                                              "cv" = as.numeric(df[9,7]),
                                              "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[9,8]),
                                              "cv" = as.numeric(df[9,9])
                                            )
                                          }))

#View(riodoce)

########################################################
### Central de Minas (linha: 10):

central<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[10, 10]),
                                              "Pessoas com mais de 14 anos na força de trabalho" = as.numeric(df[10,2]),
                                              "cv" = as.numeric(df[10,3]),
                                              "Pessoas com mais de 14 anos na força de trabalho e ocupadas" = as.numeric(df[10, 4]),
                                              "cv" = as.numeric(df[10,5]),
                                              "Pessoas com mais de 14 anos na força de trabalho e desocupadas" = as.numeric(df[10, 6]),
                                              "cv" = as.numeric(df[10,7]),
                                              "Pessoas com mais de 14 anos, fora da força de trabalho" = as.numeric(df[10,8]),
                                              "cv" = as.numeric(df[10,9])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatual4092<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                      "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                      "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                      "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatual4092,file = "C:/Users/italo/Desktop/MICRODADOS/baseestratatual4092.rds")


