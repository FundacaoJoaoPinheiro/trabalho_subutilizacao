################################################################################
##    LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG     ##
################################################################################

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

#Tabela 6371 - Média de horas habitualmente trabalhadas por semana e efetivamente trabalhadas na semana de referência, no trabalho principal e em todos os trabalhos, das pessoas de 14 anos ou mais de idade, por sexo
library(PNADcIBGE)
library(survey)
library(tidyverse)

setwd("C:/Users/italo/Desktop/trimestral")

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
calcula_ocup_desocup <- function(mesano){
  pnadc <- pnadc_design(read_pnadc(paste0("C:/Users/italo/Desktop/trimestral/txt/PNADC_0",mesano,".txt"), 
                                   "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "VD4010", "VD4032", "VD4031", "VD4035", "VD4011", "VD4007", "VD4009", "VD4001", "V2009", "VD4001", "V2007", "V2010", "VD3004", "V4076"))) %>%
    
    update(
      #Criação das variáveis para população, conforme variáveis
      pia = as.numeric(V2009 >= 14),
      
      rend2 = as.numeric(VD4031),
      rend3 = as.numeric(VD4035),
      
      #14 a 17
      horastodoshabit_1 = ifelse(pia %in% 1  & (V2009 >= 14 & V2009 <= 17), rend2, NA),
      horastodosefetiv_1 = ifelse(pia %in% 1 & (V2009 >= 14 & V2009 <= 17), rend3, NA),
      
      #18 a 24
      horastodoshabit_2 = ifelse(pia %in% 1  & (V2009 >= 18 & V2009 <= 24), rend2, NA),
      horastodosefetiv_2 = ifelse(pia %in% 1 & (V2009 >= 18 & V2009 <= 24), rend3, NA),
      
      #25 a 39
      horastodoshabit_3 = ifelse(pia %in% 1  & (V2009 >= 25 & V2009 <= 39), rend2, NA),
      horastodosefetiv_3 = ifelse(pia %in% 1 & (V2009 >= 25 & V2009 <= 39), rend3, NA),
      
      #40 a 59
      horastodoshabit_4 = ifelse(pia %in% 1  & (V2009 >= 40 & V2009 <= 59), rend2, NA),
      horastodosefetiv_4 = ifelse(pia %in% 1 & (V2009 >= 40 & V2009 <= 59), rend3, NA),
      
      #60 ou mais
      horastodoshabit_5 = ifelse(pia %in% 1  & (V2009 >= 60), rend2, NA),
      horastodosefetiv_5 = ifelse(pia %in% 1 & (V2009 >= 60), rend3, NA),
    
      #Totais
      one1 = ifelse(pia %in% 1, rend2, NA),
      one2 = ifelse(pia %in% 1, rend3, NA),

        

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
  
  ## Total
  estimativas_t1 <- svyby(~one1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t1)[3]<-"cv_t1"
  
  ## Total - Todos Habitual
  estimativas_1 <- svyby(~horastodoshabit_1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_1)[3]<-"cv_1"
  
  ## Total - Todos Habitual
  estimativas_2 <- svyby(~horastodoshabit_2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_2)[3]<-"cv_2"
  
  ## Total - Todos Habitual
  estimativas_3 <- svyby(~horastodoshabit_3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_3)[3]<-"cv_3"
  
  ## Total - Todos Habitual
  estimativas_4 <- svyby(~horastodoshabit_4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_4)[3]<-"cv_4"
  
  ## Total - Todos Habitual
  estimativas_5 <- svyby(~horastodoshabit_5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_5)[3]<-"cv_5"
  
  ## Total
  estimativas_t2 <- svyby(~one2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t2)[3]<-"cv_t2"
  
  ## Total - Todos Efetivo
  estimativas_6 <- svyby(~horastodosefetiv_1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_6)[3]<-"cv_6"
  
  ## Total - Todos Efetivo
  estimativas_7 <- svyby(~horastodosefetiv_2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_7)[3]<-"cv_7"
  
  ## Total - Todos Efetivo
  estimativas_8 <- svyby(~horastodosefetiv_3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_8)[3]<-"cv_8"

  ## Total - Todos Efetivo
  estimativas_9 <- svyby(~horastodosefetiv_4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_9)[3]<-"cv_9"
  
  ## Total - Todos Efetivo
  estimativas_10 <- svyby(~horastodosefetiv_5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_10)[3]<-"cv_10"
  
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_t1<- svymean(~one1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t2<- svymean(~one2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_1<- svymean(~horastodoshabit_1, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_2<- svymean(~horastodoshabit_2, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_3<- svymean(~horastodoshabit_3, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_4<- svymean(~horastodoshabit_4, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_5<- svymean(~horastodoshabit_5, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_6<- svymean(~horastodosefetiv_1, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_7<- svymean(~horastodosefetiv_2, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_8<- svymean(~horastodosefetiv_3, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_9<- svymean(~horastodosefetiv_4, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_10<- svymean(~horastodosefetiv_5, subset(pnadc,UF=="31"), na.rm = TRUE)
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    
    one1 = coef(total_t1),
    cv_t1 = round(cv(total_t1)*100,1),
    
    horastodoshabit_1 = coef(total_1),
    cv_1 = round(cv(total_1)*100,1),
    
    horastodoshabit_2 = coef(total_2),
    cv_2 = round(cv(total_2)*100,1),
    
    horastodoshabit_3 = coef(total_3),
    cv_3 = round(cv(total_3)*100,1),
    
    horastodoshabit_4 = coef(total_4),
    cv_4 = round(cv(total_4)*100,1),
    
    horastodoshabit_5 = coef(total_5),
    cv_5 = round(cv(total_5)*100,1),
    
    
    one2 = coef(total_t2),
    cv_t2 = round(cv(total_t2)*100,1),
    
    horastodosefetiv_1 = coef(total_6),
    cv_6 = round(cv(total_6)*100,1),
    
    horastodosefetiv_2 = coef(total_7),
    cv_7 = round(cv(total_7)*100,1),
    
    horastodosefetiv_3 = coef(total_8),
    cv_8 = round(cv(total_8)*100,1),
    
    horastodosefetiv_4 = coef(total_9),
    cv_9 = round(cv(total_9)*100,1),
    
    horastodosefetiv_5 = coef(total_10),
    cv_10 = round(cv(total_10)*100,1),
    
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas_t1 %>%
    left_join(estimativas_1) %>%
    left_join(estimativas_2) %>%
    left_join(estimativas_3) %>%
    left_join(estimativas_4) %>%
    left_join(estimativas_5) %>%
    left_join(estimativas_t2) %>%
    left_join(estimativas_6) %>%
    left_join(estimativas_7) %>%
    left_join(estimativas_8) %>%
    left_join(estimativas_9) %>%
    left_join(estimativas_10) %>%

    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_t1, estimativas_t2, estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, pnadc)
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
##          CRIANDO OS DFS PARA OS 10 ESTRATOS - 2012 A 2023                  ##
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
                                         "Período" = as.character(df[11, 26]),
                                         "Total - habitualmente em todos os trabalhos" = as.numeric(df[11,2]),
                                         "cv" = as.numeric(df[11,3]),
                                         "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[11,4]),
                                         "cv" = as.numeric(df[11,5]),
                                         "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[11, 6]),
                                         "cv" = as.numeric(df[11,7]),
                                         "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[11, 8]),
                                         "cv" = as.numeric(df[11,9]),
                                         "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[11,10]),
                                         "cv" = as.numeric(df[11,11]),
                                         "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[11,12]),
                                         "cv" = as.numeric(df[11,13]),
                                         "Total - efetivamente em todos os trabalhos" = as.numeric(df[11,14]),
                                         "cv" = as.numeric(df[11,15]),
                                         "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[11,16]),
                                         "cv" = as.numeric(df[11,17]),
                                         "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[11, 18]),
                                         "cv" = as.numeric(df[11,19]),
                                         "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[11, 20]),
                                         "cv" = as.numeric(df[11,21]),
                                         "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[11,22]),
                                         "cv" = as.numeric(df[11,23]),
                                         "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[11,24]),
                                         "cv" = as.numeric(df[11,25])
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
                                         "Período" = as.character(df[1, 26]),
                                         "Total - habitualmente em todos os trabalhos" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[1,4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[1, 6]),
                                         "cv" = as.numeric(df[1,7]),
                                         "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[1, 8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[1,10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[1,12]),
                                         "cv" = as.numeric(df[1,13]),
                                         "Total - efetivamente em todos os trabalhos" = as.numeric(df[1,14]),
                                         "cv" = as.numeric(df[1,15]),
                                         "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[1,16]),
                                         "cv" = as.numeric(df[1,17]),
                                         "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[1, 18]),
                                         "cv" = as.numeric(df[1,19]),
                                         "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[1, 20]),
                                         "cv" = as.numeric(df[1,21]),
                                         "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[1,22]),
                                         "cv" = as.numeric(df[1,23]),
                                         "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[1,24]),
                                         "cv" = as.numeric(df[1,25])
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
                                                "Período" = as.character(df[2, 26]),
                                                "Total - habitualmente em todos os trabalhos" = as.numeric(df[2,2]),
                                                "cv" = as.numeric(df[2,3]),
                                                "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[2,4]),
                                                "cv" = as.numeric(df[2,5]),
                                                "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[2, 6]),
                                                "cv" = as.numeric(df[2,7]),
                                                "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[2, 8]),
                                                "cv" = as.numeric(df[2,9]),
                                                "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[2,10]),
                                                "cv" = as.numeric(df[2,11]),
                                                "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[2,12]),
                                                "cv" = as.numeric(df[2,13]),
                                                "Total - efetivamente em todos os trabalhos" = as.numeric(df[2,14]),
                                                "cv" = as.numeric(df[2,15]),
                                                "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[2,16]),
                                                "cv" = as.numeric(df[2,17]),
                                                "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[2, 18]),
                                                "cv" = as.numeric(df[2,19]),
                                                "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[2, 20]),
                                                "cv" = as.numeric(df[2,21]),
                                                "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[2,22]),
                                                "cv" = as.numeric(df[2,23]),
                                                "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[2,24]),
                                                "cv" = as.numeric(df[2,25])
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
                                              "Período" = as.character(df[3, 26]),
                                              "Total - habitualmente em todos os trabalhos" = as.numeric(df[3,2]),
                                              "cv" = as.numeric(df[3,3]),
                                              "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[3,4]),
                                              "cv" = as.numeric(df[3,5]),
                                              "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[3, 6]),
                                              "cv" = as.numeric(df[3,7]),
                                              "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[3, 8]),
                                              "cv" = as.numeric(df[3,9]),
                                              "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[3,10]),
                                              "cv" = as.numeric(df[3,11]),
                                              "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[3,12]),
                                              "cv" = as.numeric(df[3,13]),
                                              "Total - efetivamente em todos os trabalhos" = as.numeric(df[3,14]),
                                              "cv" = as.numeric(df[3,15]),
                                              "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[3,16]),
                                              "cv" = as.numeric(df[3,17]),
                                              "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[3, 18]),
                                              "cv" = as.numeric(df[3,19]),
                                              "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[3, 20]),
                                              "cv" = as.numeric(df[3,21]),
                                              "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[3,22]),
                                              "cv" = as.numeric(df[3,23]),
                                              "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[3,24]),
                                              "cv" = as.numeric(df[3,25])
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
                                           "Período" = as.character(df[4, 26]),
                                           "Total - habitualmente em todos os trabalhos" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[4,4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[4, 6]),
                                           "cv" = as.numeric(df[4,7]),
                                           "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[4, 8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[4,10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[4,12]),
                                           "cv" = as.numeric(df[4,13]),
                                           "Total - efetivamente em todos os trabalhos" = as.numeric(df[4,14]),
                                           "cv" = as.numeric(df[4,15]),
                                           "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[4,16]),
                                           "cv" = as.numeric(df[4,17]),
                                           "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[4, 18]),
                                           "cv" = as.numeric(df[4,19]),
                                           "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[4, 20]),
                                           "cv" = as.numeric(df[4,21]),
                                           "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[4,22]),
                                           "cv" = as.numeric(df[4,23]),
                                           "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[4,24]),
                                           "cv" = as.numeric(df[4,25])
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
                                            "Período" = as.character(df[5, 26]),
                                            "Total - habitualmente em todos os trabalhos" = as.numeric(df[5,2]),
                                            "cv" = as.numeric(df[5,3]),
                                            "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[5,4]),
                                            "cv" = as.numeric(df[5,5]),
                                            "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[5, 6]),
                                            "cv" = as.numeric(df[5,7]),
                                            "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[5, 8]),
                                            "cv" = as.numeric(df[5,9]),
                                            "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[5,10]),
                                            "cv" = as.numeric(df[5,11]),
                                            "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[5,12]),
                                            "cv" = as.numeric(df[5,13]),
                                            "Total - efetivamente em todos os trabalhos" = as.numeric(df[5,14]),
                                            "cv" = as.numeric(df[5,15]),
                                            "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[5,16]),
                                            "cv" = as.numeric(df[5,17]),
                                            "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[5, 18]),
                                            "cv" = as.numeric(df[5,19]),
                                            "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[5, 20]),
                                            "cv" = as.numeric(df[5,21]),
                                            "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[5,22]),
                                            "cv" = as.numeric(df[5,23]),
                                            "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[5,24]),
                                            "cv" = as.numeric(df[5,25])
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
                                           "Período" = as.character(df[6, 26]),
                                           "Total - habitualmente em todos os trabalhos" = as.numeric(df[6,2]),
                                           "cv" = as.numeric(df[6,3]),
                                           "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[6,4]),
                                           "cv" = as.numeric(df[6,5]),
                                           "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[6, 6]),
                                           "cv" = as.numeric(df[6,7]),
                                           "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[6, 8]),
                                           "cv" = as.numeric(df[6,9]),
                                           "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[6,10]),
                                           "cv" = as.numeric(df[6,11]),
                                           "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[6,12]),
                                           "cv" = as.numeric(df[6,13]),
                                           "Total - efetivamente em todos os trabalhos" = as.numeric(df[6,14]),
                                           "cv" = as.numeric(df[6,15]),
                                           "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[6,16]),
                                           "cv" = as.numeric(df[6,17]),
                                           "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[6, 18]),
                                           "cv" = as.numeric(df[6,19]),
                                           "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[6, 20]),
                                           "cv" = as.numeric(df[6,21]),
                                           "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[6,22]),
                                           "cv" = as.numeric(df[6,23]),
                                           "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[6,24]),
                                           "cv" = as.numeric(df[6,25])
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
                                               "Período" = as.character(df[7, 26]),
                                               "Total - habitualmente em todos os trabalhos" = as.numeric(df[7,2]),
                                               "cv" = as.numeric(df[7,3]),
                                               "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[7,4]),
                                               "cv" = as.numeric(df[7,5]),
                                               "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[7, 6]),
                                               "cv" = as.numeric(df[7,7]),
                                               "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[7, 8]),
                                               "cv" = as.numeric(df[7,9]),
                                               "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[7,10]),
                                               "cv" = as.numeric(df[7,11]),
                                               "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[7,12]),
                                               "cv" = as.numeric(df[7,13]),
                                               "Total - efetivamente em todos os trabalhos" = as.numeric(df[7,14]),
                                               "cv" = as.numeric(df[7,15]),
                                               "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[7,16]),
                                               "cv" = as.numeric(df[7,17]),
                                               "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[7, 18]),
                                               "cv" = as.numeric(df[7,19]),
                                               "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[7, 20]),
                                               "cv" = as.numeric(df[7,21]),
                                               "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[7,22]),
                                               "cv" = as.numeric(df[7,23]),
                                               "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[7,24]),
                                               "cv" = as.numeric(df[7,25])
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
                                              "Período" = as.character(df[8, 26]),
                                              "Total - habitualmente em todos os trabalhos" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[8,4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[8, 6]),
                                              "cv" = as.numeric(df[8,7]),
                                              "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[8, 8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[8,10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[8,12]),
                                              "cv" = as.numeric(df[8,13]),
                                              "Total - efetivamente em todos os trabalhos" = as.numeric(df[8,14]),
                                              "cv" = as.numeric(df[8,15]),
                                              "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[8,16]),
                                              "cv" = as.numeric(df[8,17]),
                                              "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[8, 18]),
                                              "cv" = as.numeric(df[8,19]),
                                              "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[8, 20]),
                                              "cv" = as.numeric(df[8,21]),
                                              "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[8,22]),
                                              "cv" = as.numeric(df[8,23]),
                                              "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[8,24]),
                                              "cv" = as.numeric(df[8,25])
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
                                              "Período" = as.character(df[9, 26]),
                                              "Total - habitualmente em todos os trabalhos" = as.numeric(df[9,2]),
                                              "cv" = as.numeric(df[9,3]),
                                              "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[9,4]),
                                              "cv" = as.numeric(df[9,5]),
                                              "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[9, 6]),
                                              "cv" = as.numeric(df[9,7]),
                                              "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[9, 8]),
                                              "cv" = as.numeric(df[9,9]),
                                              "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[9,10]),
                                              "cv" = as.numeric(df[9,11]),
                                              "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[9,12]),
                                              "cv" = as.numeric(df[9,13]),
                                              "Total - efetivamente em todos os trabalhos" = as.numeric(df[9,14]),
                                              "cv" = as.numeric(df[9,15]),
                                              "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[9,16]),
                                              "cv" = as.numeric(df[9,17]),
                                              "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[9, 18]),
                                              "cv" = as.numeric(df[9,19]),
                                              "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[9, 20]),
                                              "cv" = as.numeric(df[9,21]),
                                              "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[9,22]),
                                              "cv" = as.numeric(df[9,23]),
                                              "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[9,24]),
                                              "cv" = as.numeric(df[9,25])
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
                                              "Período" = as.character(df[10, 26]),
                                              "Total - habitualmente em todos os trabalhos" = as.numeric(df[10,2]),
                                              "cv" = as.numeric(df[10,3]),
                                              "Habitualmente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[10,4]),
                                              "cv" = as.numeric(df[10,5]),
                                              "Habitualmente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[10, 6]),
                                              "cv" = as.numeric(df[10,7]),
                                              "Habitualmente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[10, 8]),
                                              "cv" = as.numeric(df[10,9]),
                                              "Habitualmente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[10,10]),
                                              "cv" = as.numeric(df[10,11]),
                                              "Habitualmente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[10,12]),
                                              "cv" = as.numeric(df[10,13]),
                                              "Total - efetivamente em todos os trabalhos" = as.numeric(df[10,14]),
                                              "cv" = as.numeric(df[10,15]),
                                              "Efetivamente em todos os trabalhos - 14 a 17 anos" = as.numeric(df[10,16]),
                                              "cv" = as.numeric(df[10,17]),
                                              "Efetivamente em todos os trabalhos - 18 a 24 anos" = as.numeric(df[10, 18]),
                                              "cv" = as.numeric(df[10,19]),
                                              "Efetivamente em todos os trabalhos - 25 a 39 anos" = as.numeric(df[10, 20]),
                                              "cv" = as.numeric(df[10,21]),
                                              "Efetivamente em todos os trabalhos - 40 a 59 anos" = as.numeric(df[10,22]),
                                              "cv" = as.numeric(df[10,23]),
                                              "Efetivamente em todos os trabalhos - 60 anos ou mais" = as.numeric(df[10,24]),
                                              "cv" = as.numeric(df[10,25])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualV6372<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                           "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                           "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                           "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualV6372,file = "C:/Users/italo/Desktop/trimestral/baseestratatualV6372.rds")

