################################################################################
## LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG        ##
################################################################################

## Tabela 5917 - População, por sexo

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

library(PNADcIBGE)
library(survey)
library(tidyverse)

setwd("C:/Users/italo/Desktop/trimestral")

## Para que a função funcione, será necessário criar o caminho data/documentacao dentro do diretório principal
## Fazer isso uma única vez.
## Feitos os caminhos, mova o input para "caminhoprincipal/data/documentacao" e os dados para "caminhoprincipal/data/txt"
## Código para criar os caminhos (está comentado, para que esse código não rode toda vez):

# dir.create("data/documentacao", recursive = TRUE)
# input <- "data/documentacao/input_PNADC_anual.txt"
# print(file.exists(arquivo_dicionario)) 
# Se retornar TRUE, o código deu certo. O arquivo input está no lugar certo

# dir.create("c:/MICRODADOS/data/txt", recursive = TRUE)
# dados <- "data/txt/PNADC_2012.txt"
# print(file.exists(dados))
# Se retornar TRUE, o código deu certo. O arquivo TXT está no lugar certo

# Código para criar o caminho que armazena as bases em .RDS:

#dir.create("C:/Users/italo/Desktop/MICRODADOS/estimativas", recursive = TRUE)


## Testando com as bases no diretório
## Foram feitos os downloads de todos os arquivos .txt da PNADC no diretório

## Função que automatiza a chamada dos dados anuais, de 2012 até 2023
## Tudo uma única função

## Alterando a chamada dos estratos -> utilizando %in%
calcula_ocup_desocup <- function(mesano){
  pnadc <- pnadc_design(read_pnadc(paste0("C:/Users/italo/Desktop/trimestral/txt/PNADC_0",mesano,".txt"), 
                                   "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "V2009", "VD4001", "V2007", "V2010"))) %>%
    update(
           #A variável V2007 identifica o sexo. 1 para homem e 2 para mulher
           #Criando as variáveis para população ocupada e desocupada, de acordo com o sexo
           ocupadah = 1 * (VD4002 == 1 & V2007 == 1),
           desocupadah = 1 * (VD4002 == 2 & V2007 == 1),
           ocupadam = 1 * (VD4002 == 1 & V2007 == 2),
           desocupadam = 1 * (VD4002 == 2 & V2007 == 2),
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
  
  ## Total de homens ocupados
  estimativas_oh <- svyby(~ocupadah, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_oh)[3]<-"cv_1"
  
  ## Total de homens desocupados
  estimativas_dh <- svyby(~desocupadah, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_dh)[3]<-"cv_2"
  
  ## Total de mulheres ocupadas
  estimativas_om <- svyby(~ocupadam, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_om)[3]<-"cv_3"
  
  ## Total de mulheres desocupados
  estimativas_dm <- svyby(~desocupadam, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_dm)[3]<-"cv_4"
  
  ## Taxa de desocupação - Homens
  estimativas_tdh <- svyby(~desocupadah, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_tdh)[2]<-"tx_desocupada_homem"
  colnames(estimativas_tdh)[3]<-"cv_5"
  
  ## Taxa de desocupação - Mulheres
  estimativas_tdm <- svyby(~desocupadam, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_tdm)[2]<-"tx_desocupada_mulher"
  colnames(estimativas_tdm)[3]<-"cv_6"
  
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_ocupadah<- svytotal(~ocupadah, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupadam<- svytotal(~ocupadam, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_desocupadah<- svytotal(~desocupadah, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_desocupadam<- svytotal(~desocupadam, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_tx_desocupacaoh <- svymean(~desocupadah, subset(pnadc, UF == "31"), na.rm = TRUE)
  total_tx_desocupacaom <- svymean(~desocupadam, subset(pnadc, UF == "31"), na.rm = TRUE)
  
  
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    ocupadah = coef(total_ocupadah),
    cv_1 = SE(total_ocupadah),
    ocupadam = coef(total_ocupadam),
    cv_2 = SE(total_ocupadam),
    desocupadah = coef(total_desocupadah),
    cv_3 = SE(total_desocupadah),
    desocupadam = coef(total_desocupadam),
    cv_4 = SE(total_desocupadam),
    tx_desocupada_homem = coef(total_tx_desocupacaoh),
    cv_5 = SE(total_tx_desocupacaoh),
    tx_desocupada_mulher = coef(total_tx_desocupacaom),
    cv_6 = SE(total_tx_desocupacaom),
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas_oh %>% 
    left_join(estimativas_dh) %>% 
    left_join(estimativas_om) %>%
    left_join(estimativas_dm) %>%
    left_join(estimativas_tdh) %>%
    left_join(estimativas_tdm) %>%
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_oh, estimativas_dh, estimativas_om, estimativas_dm, estimativas_tdh, estimativas_tdm,  pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas,paste0("C:/Users/italo/Desktop/trimestral/estimativas/resultados_0",mesano,".RDS"))
  paste("Concluído:",mesano)
  
}
## Fim da função

## Atualizando lista até 2023:
## Continuando a formatação por colunas

lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
           022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
           032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
           042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023)

sapply(lista, function(i) calcula_ocup_desocup(i))



#Calcular os coeficientes de variação 
################################################################################
##          CRIANDO OS DATA FRAMES PARA OS 10 ESTRATOS - 2012 A 2023          ##
################################################################################


# Diretório principal
setwd("C:/Users/italo/Desktop/trimestral")

library(PNADcIBGE)
library(survey)
library(tidyverse)

## Lendo todos os arquivos .RDS
## Lembrete: o caminho deles está dentro do diretório principal
arquivos <- list.files("C:/Users/italo/Desktop/trimestral/estimativas", pattern = "\\.RDS$", full.names = TRUE)
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

mg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                     2,15,28,41,
                                     3,16,29,42,
                                     4,17,30,43,
                                     5,18,31,44,
                                     6,19,32,45,
                                     7,20,33,46,
                                     8,21,34,47,
                                     9,22,35,48,
                                     10,23,36,49,
                                     11,24,37,50,
                                     12,25,38,51,
                                     13,26,39)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[11, 14]),
                                         "Total de homens ocupados" = as.numeric(df[11,2]),
                                         "cv" = as.numeric(df[11,3]),
                                         "Total de homens desocupados" = as.numeric(df[11, 4]),
                                         "cv" = as.numeric(df[11,5]),
                                         "Taxa de desocupação - Homens" = as.numeric(df[11, 10]),
                                         "cv" = as.numeric(df[11,11]),
                                         "Total de mulheres ocupadas" = as.numeric(df[11,6]),
                                         "cv" = as.numeric(df[11,7]),
                                         "Total de mulheres desocupadas" = as.numeric(df[11, 8]),
                                         "cv" = as.numeric(df[11,9]),
                                         "Taxa de desocupação - Mulheres" = as.numeric(df[11, 12]),
                                         "cv" = as.numeric(df[11,13])
                                       )
                                     }))
#View(mg)


################################
### Belo Horizonte (linha: 1)

bh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                     2,15,28,41,
                                     3,16,29,42,
                                     4,17,30,43,
                                     5,18,31,44,
                                     6,19,32,45,
                                     7,20,33,46,
                                     8,21,34,47,
                                     9,22,35,48,
                                     10,23,36,49,
                                     11,24,37,50,
                                     12,25,38,51,
                                     13,26,39)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1, 14]),
                                         "Total de homens ocupados" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Total de homens desocupados" = as.numeric(df[1, 4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Taxa de desocupação - Homens" = as.numeric(df[1, 10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Total de mulheres ocupadas" = as.numeric(df[1,6]),
                                         "cv" = as.numeric(df[1,7]),
                                         "Total de mulheres desocupadas" = as.numeric(df[1, 8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Taxa de desocupação - Mulheres" = as.numeric(df[1, 12]),
                                         "cv" = as.numeric(df[1,13])
                                       )
                                     }))

#View(bh)

######################################
### Entorno Metropolitano de BH (linha: 2)

entornobh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                            2,15,28,41,
                                            3,16,29,42,
                                            4,17,30,43,
                                            5,18,31,44,
                                            6,19,32,45,
                                            7,20,33,46,
                                            8,21,34,47,
                                            9,22,35,48,
                                            10,23,36,49,
                                            11,24,37,50,
                                            12,25,38,51,
                                            13,26,39)], function(df) {
                                              data.frame(
                                              "Período" = as.character(df[2, 14]),
                                              "Total de homens ocupados" = as.numeric(df[2,2]),
                                              "cv" = as.numeric(df[2,3]),
                                              "Total de homens desocupados" = as.numeric(df[2,4]),
                                              "cv" = as.numeric(df[2,5]),
                                              "Taxa de desocupação - Homens" = as.numeric(df[2,10]),
                                              "cv" = as.numeric(df[2,11]),
                                              "Total de mulheres ocupadas" = as.numeric(df[2,6]),
                                              "cv" = as.numeric(df[2,7]),
                                              "Total de mulheres desocupadas" = as.numeric(df[2,8]),
                                              "cv" = as.numeric(df[2,9]),
                                              "Taxa de desocupação - Mulheres" = as.numeric(df[2,12]),
                                              "cv" = as.numeric(df[2,13])
                                              )
                                            }))

#View(entornobh)

########################################
### Colar Metropolitano de BH (linha: 3)

colarbh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 14]),
                                              "Total de homens ocupados" = as.numeric(df[3,2]),
                                              "cv" = as.numeric(df[3,3]),
                                              "Total de homens desocupados" = as.numeric(df[3, 4]),
                                              "cv" = as.numeric(df[3,5]),
                                              "Taxa de desocupação - Homens" = as.numeric(df[3, 10]),
                                              "cv" = as.numeric(df[3,11]),
                                              "Total de mulheres ocupadas" = as.numeric(df[3,6]),
                                              "cv" = as.numeric(df[3,7]),
                                              "Total de mulheres desocupadas" = as.numeric(df[3, 8]),
                                              "cv" = as.numeric(df[3,9]),
                                              "Taxa de desocupação - Mulheres" = as.numeric(df[3, 12]),
                                              "cv" = as.numeric(df[3,13])
                                            )
                                          }))

#View(colarbh)

########################################
### RIDE de Brasília em Minas (linha: 4)

RIDE<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                       2,15,28,41,
                                       3,16,29,42,
                                       4,17,30,43,
                                       5,18,31,44,
                                       6,19,32,45,
                                       7,20,33,46,
                                       8,21,34,47,
                                       9,22,35,48,
                                       10,23,36,49,
                                       11,24,37,50,
                                       12,25,38,51,
                                       13,26,39)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 14]),
                                           "Total de homens ocupados" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Total de homens desocupados" = as.numeric(df[4, 4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Taxa de desocupação - Homens" = as.numeric(df[4, 10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Total de mulheres ocupadas" = as.numeric(df[4,6]),
                                           "cv" = as.numeric(df[4,7]),
                                           "Total de mulheres desocupadas" = as.numeric(df[4, 8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Taxa de desocupação - Mulheres" = as.numeric(df[4, 12]),
                                           "cv" = as.numeric(df[4,13])
                                         )
                                       }))

#View(RIDE)

#####################################
### Sul de Minas (linha: 5)
sulmg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                        2,15,28,41,
                                        3,16,29,42,
                                        4,17,30,43,
                                        5,18,31,44,
                                        6,19,32,45,
                                        7,20,33,46,
                                        8,21,34,47,
                                        9,22,35,48,
                                        10,23,36,49,
                                        11,24,37,50,
                                        12,25,38,51,
                                        13,26,39)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[5, 14]),
                                            "Total de homens ocupados" = as.numeric(df[5,2]),
                                            "cv" = as.numeric(df[5,3]),
                                            "Total de homens desocupados" = as.numeric(df[5, 4]),
                                            "cv" = as.numeric(df[5,5]),
                                            "Taxa de desocupação - Homens" = as.numeric(df[5, 10]),
                                            "cv" = as.numeric(df[5,11]),
                                            "Total de mulheres ocupadas" = as.numeric(df[5,6]),
                                            "cv" = as.numeric(df[5,7]),
                                            "Total de mulheres desocupadas" = as.numeric(df[5, 8]),
                                            "cv" = as.numeric(df[5,9]),
                                            "Taxa de desocupação - Mulheres" = as.numeric(df[5, 12]),
                                            "cv" = as.numeric(df[5,13])
                                          )
                                        }))

#View(sulmg)

########################################################
### Triângulo Mineiro (linha: 6)

trng<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                       2,15,28,41,
                                       3,16,29,42,
                                       4,17,30,43,
                                       5,18,31,44,
                                       6,19,32,45,
                                       7,20,33,46,
                                       8,21,34,47,
                                       9,22,35,48,
                                       10,23,36,49,
                                       11,24,37,50,
                                       12,25,38,51,
                                       13,26,39)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[6, 14]),
                                           "Total de homens ocupados" = as.numeric(df[6,2]),
                                           "cv" = as.numeric(df[6,3]),
                                           "Total de homens desocupados" = as.numeric(df[6, 4]),
                                           "cv" = as.numeric(df[6,5]),
                                           "Taxa de desocupação - Homens" = as.numeric(df[6, 10]),
                                           "cv" = as.numeric(df[6,11]),
                                           "Total de mulheres ocupadas" = as.numeric(df[6,6]),
                                           "cv" = as.numeric(df[6,7]),
                                           "Total de mulheres desocupadas" = as.numeric(df[6, 8]),
                                           "cv" = as.numeric(df[6,9]),
                                           "Taxa de desocupação - Mulheres" = as.numeric(df[6, 12]),
                                           "cv" = as.numeric(df[6,13])
                                         )
                                       }))

#View(trng)

###############################################################
### Mata de Minas Gerais (linha: 7)

zonamata<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                           2,15,28,41,
                                           3,16,29,42,
                                           4,17,30,43,
                                           5,18,31,44,
                                           6,19,32,45,
                                           7,20,33,46,
                                           8,21,34,47,
                                           9,22,35,48,
                                           10,23,36,49,
                                           11,24,37,50,
                                           12,25,38,51,
                                           13,26,39)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[7, 14]),
                                               "Total de homens ocupados" = as.numeric(df[7,2]),
                                               "cv" = as.numeric(df[7,3]),
                                               "Total de homens desocupados" = as.numeric(df[7, 4]),
                                               "cv" = as.numeric(df[7,5]),
                                               "Taxa de desocupação - Homens" = as.numeric(df[7, 10]),
                                               "cv" = as.numeric(df[7,11]),
                                               "Total de mulheres ocupadas" = as.numeric(df[7,6]),
                                               "cv" = as.numeric(df[7,7]),
                                               "Total de mulheres desocupadas" = as.numeric(df[7, 8]),
                                               "cv" = as.numeric(df[7,9]),
                                               "Taxa de desocupação - Mulheres" = as.numeric(df[7, 12]),
                                               "cv" = as.numeric(df[7,13])
                                             )
                                           }))

#View(zonamata)

############################################################
### Norte de Minas (Linha: 8)

nortemg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8, 14]),
                                              "Total de homens ocupados" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Total de homens desocupados" = as.numeric(df[8, 4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Taxa de desocupação - Homens" = as.numeric(df[8, 10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Total de mulheres ocupadas" = as.numeric(df[8,6]),
                                              "cv" = as.numeric(df[8,7]),
                                              "Total de mulheres desocupadas" = as.numeric(df[8, 8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Taxa de desocupação - Mulheres" = as.numeric(df[8, 12]),
                                              "cv" = as.numeric(df[8,13])
                                            )
                                          }))

#View(nortemg)

######################################################
### Vale do Rio Doce (linha: 9)

riodoce<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[9, 14]),
                                              "Total de homens ocupados" = as.numeric(df[9,2]),
                                              "cv" = as.numeric(df[9,3]),
                                              "Total de homens desocupados" = as.numeric(df[9, 4]),
                                              "cv" = as.numeric(df[9,5]),
                                              "Taxa de desocupação - Homens" = as.numeric(df[9, 10]),
                                              "cv" = as.numeric(df[9,11]),
                                              "Total de mulheres ocupadas" = as.numeric(df[9,6]),
                                              "cv" = as.numeric(df[9,7]),
                                              "Total de mulheres desocupadas" = as.numeric(df[9, 8]),
                                              "cv" = as.numeric(df[9,9]),
                                              "Taxa de desocupação - Mulheres" = as.numeric(df[9, 12]),
                                              "cv" = as.numeric(df[9,13])
                                            )
                                          }))

#View(riodoce)

########################################################
### Central de Minas (linha: 10):

central<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[10, 14]),
                                              "Total de homens ocupados" = as.numeric(df[10,2]),
                                              "cv" = as.numeric(df[10,3]),
                                              "Total de homens desocupados" = as.numeric(df[10, 4]),
                                              "cv" = as.numeric(df[10,5]),
                                              "Taxa de desocupação - Homens" = as.numeric(df[10, 10]),
                                              "cv" = as.numeric(df[10,11]),
                                              "Total de mulheres ocupadas" = as.numeric(df[10,6]),
                                              "cv" = as.numeric(df[10,7]),
                                              "Total de mulheres desocupadas" = as.numeric(df[10, 8]),
                                              "cv" = as.numeric(df[10,9]),
                                              "Taxa de desocupação - Mulheres" = as.numeric(df[10, 12]),
                                              "cv" = as.numeric(df[10,13])
                                            )
                                          }))

#View(central)

### SALVANDO O ARQUIVO LISTA - CONTÉM OS 11 DATA FRAMES #####################################################
baseestratatualsex<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                      "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                      "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                      "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualsex,file = "C:/Users/italo/Desktop/trimestral/baseestratatualsex.rds")

