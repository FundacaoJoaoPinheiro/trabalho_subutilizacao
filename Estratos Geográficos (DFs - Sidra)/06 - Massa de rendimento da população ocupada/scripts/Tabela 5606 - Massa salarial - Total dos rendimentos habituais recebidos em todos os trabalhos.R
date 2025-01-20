################################################################################
## LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG        ##
################################################################################

#Tabela 5606 - Total dos rendimentos habituais, recebidos em todos os trabalhos

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
                                   paste0("C:/Users/italo/Desktop/MICRODADOS/doc/input_PNADC_",ano,".txt"),vars=c("VD4002","V2007", "V2009", "V2010", "VD4019", "VD4015"))) %>% 
    update(
           #criando variável para armazenar a renda, se o indivíduo está ocupado
           pia = as.numeric(V2009 >= 14 ),
           ocupados = ifelse( pia == 1 , as.numeric( VD4002 %in% 1 ) , NA),
           VD4019n = ifelse(ocupados %in% 1, VD4019, NA),
           
           d.homem = 1 * (V2007 == "Homem"),
           d.mulher = 1 * (V2007 == "Mulher"),
           
           raca_cor = case_when(
             V2010 == "Preta" | V2010 == "Parda" ~ "Preta ou parda",
             V2010 == "Amarela"  |
               V2010 == "Indígena" | V2010 == "Ignorado" ~ "Outras",
             V2010 == "Branca" ~ "Branca"
           ),
           
           faixa_idade = case_when((V2009 >= 14 &
                                      V2009 <= 29) ~ "0Preta ou Pardas",
                                   (V2009 >= 30 &
                                      V2009 <= 44) ~ "Amarela",
                                   (V2009 >= 45 &
                                      V2009 <= 59) ~ "Indígena",
                                   (V2009 >= 60) ~ "Branca"
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
  
  ## Total - Massa Salarial
  estimativas_1 <- svyby(~VD4019n, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_1)[3]<-"cv"
  
  
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_ocupada1<- svytotal(~VD4019n, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    ocupada1 = coef(total_ocupada1),
    #se_1 = SE(total_ocupada1),
    periodo = paste0(ano)
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas_1 %>% 
    mutate(periodo = paste0(ano)) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_1, pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas,paste0("C:/Users/italo/Desktop/MICRODADOS/estimativas/resultados_",ano,".RDS"))
  paste("Concluído:",ano)
  
}
## Fim da função

## Atualizando lista até 2023:
## Continuando a formatação por colunas

lista <- c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)

sapply(lista, function(i) calcula_ocup_desocup(i))

## Testando leitura de um arquivo

pnad2023<-readRDS("C:/Users/italo/Desktop/MICRODADOS/estimativas/resultados_2023.RDS")
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
                                         "Período" = as.character(df[11, 4]),
                                         "Massa salarial -  (R$) " = as.numeric(df[11,2]),
                                         "cv_1" = as.numeric(df[11,3])
                                       )
                                     }))
#View(mg)

################################
### Belo Horizonte (linha: 1)

bh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                     5,6,7,8,
                                     9,10,11,12)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1, 4]),
                                         "Massa salarial -  (R$) " = as.numeric(df[1,2]),
                                         "cv_1" = as.numeric(df[1,3])
                                       )
                                     }))

#View(bh)

######################################
### Entorno Metropolitano de BH (linha: 2)

entornobh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                            5,6,7,8,
                                            9,10,11,12)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2, 4]),
                                                "Massa salarial - (R$) " = as.numeric(df[2,2]),
                                                "cv_1" = as.numeric(df[2,3])
                                              )
                                            }))

#View(entornobh)

########################################
### Colar Metropolitano de BH (linha: 3)

colarbh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 4]),
                                              "Massa salarial - (R$) " = as.numeric(df[3,2]),
                                              "cv_1" = as.numeric(df[3,3])
                                            )
                                          }))

#View(colarbh)

########################################
### RIDE de Brasília em Minas (linha: 4)

RIDE<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 4]),
                                           "Massa salarial - (R$) " = as.numeric(df[4,2]),
                                           "cv_1" = as.numeric(df[4,3])
                                         )
                                       }))

#View(RIDE)

#####################################
### Sul de Minas (linha: 5)
sulmg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                        5,6,7,8,
                                        9,10,11,12)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[5, 4]),
                                            "Massa salarial - (R$) " = as.numeric(df[5,2]),
                                            "cv_1" = as.numeric(df[5,3])
                                          )
                                        }))

#View(sulmg)

########################################################
### Triângulo Mineiro (linha: 6)

trng<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[6, 4]),
                                           "Massa salarial -  (R$) " = as.numeric(df[6,2]),
                                           "cv_1" = as.numeric(df[6,3])
                                         )
                                       }))

#View(trng)

###############################################################
### Mata de Minas Gerais (linha: 7)

zonamata<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                           5,6,7,8,
                                           9,10,11,12)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[7, 4]),
                                               "Massa salarial - (R$) " = as.numeric(df[7,2]),
                                               "cv_1" = as.numeric(df[7,3])
                                             )
                                           }))

#View(zonamata)

############################################################
### Norte de Minas (Linha: 8)

nortemg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8, 4]),
                                              "Massa salarial - (R$) " = as.numeric(df[8,2]),
                                              "cv_1" = as.numeric(df[8,3])
                                            )
                                          }))

#View(nortemg)

######################################################
### Vale do Rio Doce (linha: 9)

riodoce<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[9, 4]),
                                              "Massa salarial - (R$) " = as.numeric(df[9,2]),
                                              "cv_1" = as.numeric(df[9,3])
                                            )
                                          }))

#View(riodoce)

########################################################
### Central de Minas (linha: 10):

central<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[10, 4]),
                                              "Massa salarial - (R$) " = as.numeric(df[10,2]),
                                              "cv_1" = as.numeric(df[10,3])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualmassa<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                      "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                      "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                      "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualmassa,file = "C:/Users/italo/Desktop/MICRODADOS/baseestratatualmassa.rds")

