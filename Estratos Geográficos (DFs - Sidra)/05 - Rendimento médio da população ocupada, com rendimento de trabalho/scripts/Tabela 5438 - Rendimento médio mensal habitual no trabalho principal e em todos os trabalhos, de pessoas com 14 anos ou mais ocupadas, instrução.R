################################################################################
##  LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG       ##
################################################################################

#Tabela 5438 - Rendimento médio mensal real das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente e efetivamente recebidos no trabalho principal e em todos os trabalhos, por instrução

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
                                   paste0("C:/Users/italo/Desktop/MICRODADOS/doc/input_PNADC_",ano,".txt"),vars=c("VD4002","V2007", "V2009", "V2010", "VD4019", "VD4015", "VD4007", "VD4016", "VD3004"))) %>% 
    update(
           #Criando as variáveis de renda
      
           pia = as.numeric(V2009 >= 14) ,
           ocupados = ifelse( pia == 1 , as.numeric( VD4002 %in% 1 ) , NA),
           renda_trabalho_princ_1 = ifelse(ocupados %in% 1 & VD3004 == 1, VD4016, NA),
           renda_trabalho_princ_2 = ifelse(ocupados %in% 1 & VD3004 == 2, VD4016, NA),
           renda_trabalho_princ_3 = ifelse(ocupados %in% 1 & VD3004 == 3, VD4016, NA),
           renda_trabalho_princ_4 = ifelse(ocupados %in% 1 & VD3004 == 4, VD4016, NA),
           renda_trabalho_princ_5 = ifelse(ocupados %in% 1 & VD3004 == 5, VD4016, NA),
           renda_trabalho_princ_6 = ifelse(ocupados %in% 1 & VD3004 == 6, VD4016, NA),
           renda_trabalho_princ_7 = ifelse(ocupados %in% 1 & VD3004 == 7, VD4016, NA),
           renda_tt_1 = ifelse(ocupados %in% 1 & VD3004 == 1, VD4019, NA),
           renda_tt_2 = ifelse(ocupados %in% 1 & VD3004 == 2, VD4019, NA),
           renda_tt_3 = ifelse(ocupados %in% 1 & VD3004 == 3, VD4019, NA),
           renda_tt_4 = ifelse(ocupados %in% 1 & VD3004 == 4, VD4019, NA),
           renda_tt_5 = ifelse(ocupados %in% 1 & VD3004 == 5, VD4019, NA),
           renda_tt_6 = ifelse(ocupados %in% 1 & VD3004 == 6, VD4019, NA),
           renda_tt_7 = ifelse(ocupados %in% 1 & VD3004 == 7, VD4019, NA),
           
           
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
  estimativas_1 <- svyby(~renda_trabalho_princ_1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_1)[3]<-"cv_1"
  
  estimativas_2 <- svyby(~renda_trabalho_princ_2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_2)[3]<-"cv_2"
  
  estimativas_3 <- svyby(~renda_trabalho_princ_3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_3)[3]<-"cv_3"

  estimativas_4 <- svyby(~renda_trabalho_princ_4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_4)[3]<-"cv_4"
  
  estimativas_5 <- svyby(~renda_trabalho_princ_5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_5)[3]<-"cv_5"
  
  estimativas_6 <- svyby(~renda_trabalho_princ_6, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_6)[3]<-"cv_6"
  
  estimativas_7 <- svyby(~renda_trabalho_princ_7, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_7)[3]<-"cv_7"
  
  estimativas_8 <- svyby(~renda_tt_1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_8)[3]<-"cv_8"
  
  estimativas_9 <- svyby(~renda_tt_2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_9)[3]<-"cv_9"
  
  estimativas_10 <- svyby(~renda_tt_3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_10)[3]<-"cv_10"
  
  estimativas_11 <- svyby(~renda_tt_4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_11)[3]<-"cv_11"
  
  estimativas_12 <- svyby(~renda_tt_5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_12)[3]<-"cv_12"
  
  estimativas_13 <- svyby(~renda_tt_6, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_13)[3]<-"cv_13"
  
  estimativas_14 <- svyby(~renda_tt_7, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_14)[3]<-"cv_14"
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_ocupada1<- svymean(~renda_trabalho_princ_1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada2<- svymean(~renda_trabalho_princ_2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada3<- svymean(~renda_trabalho_princ_3, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada4<- svymean(~renda_trabalho_princ_4, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada5<- svymean(~renda_trabalho_princ_5, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada6<- svymean(~renda_trabalho_princ_6, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada7<- svymean(~renda_trabalho_princ_7, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada8<- svymean(~renda_tt_1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada9<- svymean(~renda_tt_2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada10<- svymean(~renda_tt_3, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada11<- svymean(~renda_tt_4, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada12<- svymean(~renda_tt_5, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada13<- svymean(~renda_tt_6, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada14<- svymean(~renda_tt_7, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    ocupada1 = coef(total_ocupada1),
    #se_1 = SE(total_ocupada1),
    ocupada2 = coef(total_ocupada2),
    #se_2 = SE(total_ocupada2),
    ocupada3 = coef(total_ocupada3),
    #se_3 = SE(total_ocupada3),
    ocupada4 = coef(total_ocupada4),
    #se_4 = SE(total_ocupada4),
    ocupada5 = coef(total_ocupada5),
    #se_5 = SE(total_ocupada5),
    ocupada6 = coef(total_ocupada6),
    #se_6 = SE(total_ocupada6),
    ocupada7 = coef(total_ocupada7),
    #se_7 = SE(total_ocupada7),
    ocupada8 = coef(total_ocupada8),
    #se_8 = SE(total_ocupada8),
    ocupada9 = coef(total_ocupada9),
    #se_9 = SE(total_ocupada9),
    ocupada10 = coef(total_ocupada10),
    #se_10 = SE(total_ocupada10),
    ocupada11 = coef(total_ocupada11),
    #se_11 = SE(total_ocupada11),
    ocupada12 = coef(total_ocupada12),
    #se_12 = SE(total_ocupada12),
    ocupada13 = coef(total_ocupada13),
    #se_13 = SE(total_ocupada13),
    ocupada14 = coef(total_ocupada14),
    #se_14 = SE(total_ocupada14),
    periodo = paste0(ano)
  )
  
  
  # Juntando as bases -> regional e MG
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
    left_join(estimativas_11) %>%
    left_join(estimativas_12) %>%
    left_join(estimativas_13) %>%
    left_join(estimativas_14) %>%
    mutate(periodo = paste0(ano)) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, estimativas_12, estimativas_13, estimativas_14, pnadc)
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
                                         "Período" = as.character(df[11, 30]),
                                         "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[11,2]),
                                         "cv_1" = as.numeric(df[11,3]),
                                         "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[11,4]),
                                         "cv_2" = as.numeric(df[11,5]),
                                         "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[11,6]),
                                         "cv_3" = as.numeric(df[11,7]),
                                         "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[11,8]),
                                         "cv_4" = as.numeric(df[11,9]),
                                         "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[11,10]),
                                         "cv_5" = as.numeric(df[11,11]),
                                         "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[11,12]),
                                         "cv_6" = as.numeric(df[11,13]),
                                         "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[11,14]),
                                         "cv_7" = as.numeric(df[11,15]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[11,16]),
                                         "cv_8" = as.numeric(df[11,17]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[11,18]),
                                         "cv_9" = as.numeric(df[11,19]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[11,20]),
                                         "cv_10" = as.numeric(df[11,21]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[11,22]),
                                         "cv_11" = as.numeric(df[11,23]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[11,24]),
                                         "cv_12" = as.numeric(df[11,25]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[11,26]),
                                         "cv_13" = as.numeric(df[11,27]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[11,28]),
                                         "cv_14" = as.numeric(df[11,29])
                                       )
                                     }))
#View(mg)

################################
### Belo Horizonte (linha: 1)

bh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                     5,6,7,8,
                                     9,10,11,12)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1, 30]),
                                         "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[1,2]),
                                         "cv_1" = as.numeric(df[1,3]),
                                         "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[1,4]),
                                         "cv_2" = as.numeric(df[1,5]),
                                         "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[1,6]),
                                         "cv_3" = as.numeric(df[1,7]),
                                         "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[1,8]),
                                         "cv_4" = as.numeric(df[1,9]),
                                         "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[1,10]),
                                         "cv_5" = as.numeric(df[1,11]),
                                         "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[1,12]),
                                         "cv_6" = as.numeric(df[1,13]),
                                         "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[1,14]),
                                         "cv_7" = as.numeric(df[1,15]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[1,16]),
                                         "cv_8" = as.numeric(df[1,17]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[1,18]),
                                         "cv_9" = as.numeric(df[1,19]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[1,20]),
                                         "cv_10" = as.numeric(df[1,21]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[1,22]),
                                         "cv_11" = as.numeric(df[1,23]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[1,24]),
                                         "cv_12" = as.numeric(df[1,25]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[1,26]),
                                         "cv_13" = as.numeric(df[1,27]),
                                         "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[1,28]),
                                         "cv_14" = as.numeric(df[1,29])
                                       )
                                     }))

#View(bh)

######################################
### Entorno Metropolitano de BH (linha: 2)

entornobh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                            5,6,7,8,
                                            9,10,11,12)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2, 30]),
                                                "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[2,2]),
                                                "cv_1" = as.numeric(df[2,3]),
                                                "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[2,4]),
                                                "cv_2" = as.numeric(df[2,5]),
                                                "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[2,6]),
                                                "cv_3" = as.numeric(df[2,7]),
                                                "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[2,8]),
                                                "cv_4" = as.numeric(df[2,9]),
                                                "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[2,10]),
                                                "cv_5" = as.numeric(df[2,11]),
                                                "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[2,12]),
                                                "cv_6" = as.numeric(df[2,13]),
                                                "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[2,14]),
                                                "cv_7" = as.numeric(df[2,15]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[2,16]),
                                                "cv_8" = as.numeric(df[2,17]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[2,18]),
                                                "cv_9" = as.numeric(df[2,19]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[2,20]),
                                                "cv_10" = as.numeric(df[2,21]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[2,22]),
                                                "cv_11" = as.numeric(df[2,23]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[2,24]),
                                                "cv_12" = as.numeric(df[2,25]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[2,26]),
                                                "cv_13" = as.numeric(df[2,27]),
                                                "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[2,28]),
                                                "cv_14" = as.numeric(df[2,29])
                                              )
                                            }))

#View(entornobh)

########################################
### Colar Metropolitano de BH (linha: 3)

colarbh<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 30]),
                                              "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[3,2]),
                                              "cv_1" = as.numeric(df[3,3]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[3,4]),
                                              "cv_2" = as.numeric(df[3,5]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[3,6]),
                                              "cv_3" = as.numeric(df[3,7]),
                                              "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[3,8]),
                                              "cv_4" = as.numeric(df[3,9]),
                                              "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[3,10]),
                                              "cv_5" = as.numeric(df[3,11]),
                                              "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[3,12]),
                                              "cv_6" = as.numeric(df[3,13]),
                                              "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[3,14]),
                                              "cv_7" = as.numeric(df[3,15]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[3,16]),
                                              "cv_8" = as.numeric(df[3,17]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[3,18]),
                                              "cv_9" = as.numeric(df[3,19]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[3,20]),
                                              "cv_10" = as.numeric(df[3,21]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[3,22]),
                                              "cv_11" = as.numeric(df[3,23]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[3,24]),
                                              "cv_12" = as.numeric(df[3,25]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[3,26]),
                                              "cv_13" = as.numeric(df[3,27]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[3,28]),
                                              "cv_14" = as.numeric(df[3,29])
                                            )
                                          }))

#View(colarbh)

########################################
### RIDE de Brasília em Minas (linha: 4)

RIDE<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 30]),
                                           "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[4,2]),
                                           "cv_1" = as.numeric(df[4,3]),
                                           "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[4,4]),
                                           "cv_2" = as.numeric(df[4,5]),
                                           "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[4,6]),
                                           "cv_3" = as.numeric(df[4,7]),
                                           "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[4,8]),
                                           "cv_4" = as.numeric(df[4,9]),
                                           "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[4,10]),
                                           "cv_5" = as.numeric(df[4,11]),
                                           "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[4,12]),
                                           "cv_6" = as.numeric(df[4,13]),
                                           "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[4,14]),
                                           "cv_7" = as.numeric(df[4,15]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[4,16]),
                                           "cv_8" = as.numeric(df[4,17]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[4,18]),
                                           "cv_9" = as.numeric(df[4,19]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[4,20]),
                                           "cv_10" = as.numeric(df[4,21]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[4,22]),
                                           "cv_11" = as.numeric(df[4,23]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[4,24]),
                                           "cv_12" = as.numeric(df[4,25]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[4,26]),
                                           "cv_13" = as.numeric(df[4,27]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[4,28]),
                                           "cv_14" = as.numeric(df[4,29])
                                         )
                                       }))

#View(RIDE)

#####################################
### Sul de Minas (linha: 5)
sulmg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                        5,6,7,8,
                                        9,10,11,12)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[5, 30]),
                                            "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[5,2]),
                                            "cv_1" = as.numeric(df[5,3]),
                                            "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[5,4]),
                                            "cv_2" = as.numeric(df[5,5]),
                                            "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[5,6]),
                                            "cv_3" = as.numeric(df[5,7]),
                                            "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[5,8]),
                                            "cv_4" = as.numeric(df[5,9]),
                                            "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[5,10]),
                                            "cv_5" = as.numeric(df[5,11]),
                                            "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[5,12]),
                                            "cv_6" = as.numeric(df[5,13]),
                                            "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[5,14]),
                                            "cv_7" = as.numeric(df[5,15]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[5,16]),
                                            "cv_8" = as.numeric(df[5,17]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[5,18]),
                                            "cv_9" = as.numeric(df[5,19]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[5,20]),
                                            "cv_10" = as.numeric(df[5,21]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[5,22]),
                                            "cv_11" = as.numeric(df[5,23]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[5,24]),
                                            "cv_12" = as.numeric(df[5,25]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[5,26]),
                                            "cv_13" = as.numeric(df[5,27]),
                                            "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[5,28]),
                                            "cv_14" = as.numeric(df[5,29])
                                          )
                                        }))

#View(sulmg)

########################################################
### Triângulo Mineiro (linha: 6)

trng<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                       5,6,7,8,
                                       9,10,11,12)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[6, 30]),
                                           "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[6,2]),
                                           "cv_1" = as.numeric(df[6,3]),
                                           "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[6,4]),
                                           "cv_2" = as.numeric(df[6,5]),
                                           "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[6,6]),
                                           "cv_3" = as.numeric(df[6,7]),
                                           "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[6,8]),
                                           "cv_4" = as.numeric(df[6,9]),
                                           "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[6,10]),
                                           "cv_5" = as.numeric(df[6,11]),
                                           "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[6,12]),
                                           "cv_6" = as.numeric(df[6,13]),
                                           "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[6,14]),
                                           "cv_7" = as.numeric(df[6,15]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[6,16]),
                                           "cv_8" = as.numeric(df[6,17]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[6,18]),
                                           "cv_9" = as.numeric(df[6,19]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[6,20]),
                                           "cv_10" = as.numeric(df[6,21]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[6,22]),
                                           "cv_11" = as.numeric(df[6,23]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[6,24]),
                                           "cv_12" = as.numeric(df[6,25]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[6,26]),
                                           "cv_13" = as.numeric(df[6,27]),
                                           "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[6,28]),
                                           "cv_14" = as.numeric(df[6,29])
                                         )
                                       }))

#View(trng)

###############################################################
### Mata de Minas Gerais (linha: 7)

zonamata<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                           5,6,7,8,
                                           9,10,11,12)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[7, 30]),
                                               "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[7,2]),
                                               "cv_1" = as.numeric(df[7,3]),
                                               "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[7,4]),
                                               "cv_2" = as.numeric(df[7,5]),
                                               "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[7,6]),
                                               "cv_3" = as.numeric(df[7,7]),
                                               "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[7,8]),
                                               "cv_4" = as.numeric(df[7,9]),
                                               "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[7,10]),
                                               "cv_5" = as.numeric(df[7,11]),
                                               "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[7,12]),
                                               "cv_6" = as.numeric(df[7,13]),
                                               "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[7,14]),
                                               "cv_7" = as.numeric(df[7,15]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[7,16]),
                                               "cv_8" = as.numeric(df[7,17]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[7,18]),
                                               "cv_9" = as.numeric(df[7,19]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[7,20]),
                                               "cv_10" = as.numeric(df[7,21]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[7,22]),
                                               "cv_11" = as.numeric(df[7,23]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[7,24]),
                                               "cv_12" = as.numeric(df[7,25]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[7,26]),
                                               "cv_13" = as.numeric(df[7,27]),
                                               "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[7,28]),
                                               "cv_14" = as.numeric(df[7,29])
                                             )
                                           }))

#View(zonamata)

############################################################
### Norte de Minas (Linha: 8)

nortemg<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8, 30]),
                                              "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[8,2]),
                                              "cv_1" = as.numeric(df[8,3]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[8,4]),
                                              "cv_2" = as.numeric(df[8,5]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[8,6]),
                                              "cv_3" = as.numeric(df[8,7]),
                                              "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[8,8]),
                                              "cv_4" = as.numeric(df[8,9]),
                                              "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[8,10]),
                                              "cv_5" = as.numeric(df[8,11]),
                                              "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[8,12]),
                                              "cv_6" = as.numeric(df[8,13]),
                                              "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[8,14]),
                                              "cv_7" = as.numeric(df[8,15]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[8,16]),
                                              "cv_8" = as.numeric(df[8,17]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[8,18]),
                                              "cv_9" = as.numeric(df[8,19]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[8,20]),
                                              "cv_10" = as.numeric(df[8,21]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[8,22]),
                                              "cv_11" = as.numeric(df[8,23]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[8,24]),
                                              "cv_12" = as.numeric(df[8,25]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[8,26]),
                                              "cv_13" = as.numeric(df[8,27]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[8,28]),
                                              "cv_14" = as.numeric(df[8,29])
                                            )
                                          }))

#View(nortemg)

######################################################
### Vale do Rio Doce (linha: 9)

riodoce<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[9, 30]),
                                              "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[9,2]),
                                              "cv_1" = as.numeric(df[9,3]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[9,4]),
                                              "cv_2" = as.numeric(df[9,5]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[9,6]),
                                              "cv_3" = as.numeric(df[9,7]),
                                              "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[9,8]),
                                              "cv_4" = as.numeric(df[9,9]),
                                              "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[9,10]),
                                              "cv_5" = as.numeric(df[9,11]),
                                              "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[9,12]),
                                              "cv_6" = as.numeric(df[9,13]),
                                              "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[9,14]),
                                              "cv_7" = as.numeric(df[9,15]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[9,16]),
                                              "cv_8" = as.numeric(df[9,17]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[9,18]),
                                              "cv_9" = as.numeric(df[9,19]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[9,20]),
                                              "cv_10" = as.numeric(df[9,21]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[9,22]),
                                              "cv_11" = as.numeric(df[9,23]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[9,24]),
                                              "cv_12" = as.numeric(df[9,25]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[9,26]),
                                              "cv_13" = as.numeric(df[9,27]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[9,28]),
                                              "cv_14" = as.numeric(df[9,29])
                                            )
                                          }))

#View(riodoce)

########################################################
### Central de Minas (linha: 10):

central<-do.call(rbind, lapply(pnadcrds[c(1,2,3,4,
                                          5,6,7,8,
                                          9,10,11,12)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[10, 30]),
                                              "Rend. médio mensal habitual - Trab principal - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[10,2]),
                                              "cv_1" = as.numeric(df[10,3]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental incompleto ou equivalente" = as.numeric(df[10,4]),
                                              "cv_2" = as.numeric(df[10,5]),
                                              "Rend. médio mensal habitual - Trab principal - Fundamental completo ou equivalente" = as.numeric(df[10,6]),
                                              "cv_3" = as.numeric(df[10,7]),
                                              "Rend. médio mensal habitual - Trab principal - Médio incompleto ou equivalente" = as.numeric(df[10,8]),
                                              "cv_4" = as.numeric(df[10,9]),
                                              "Rend. médio mensal habitual - Trab principal - Médio completo ou equivalente" = as.numeric(df[10,10]),
                                              "cv_5" = as.numeric(df[10,11]),
                                              "Rend. médio mensal habitual - Trab principal - Superior incompleto ou equivalente" = as.numeric(df[10,12]),
                                              "cv_6" = as.numeric(df[10,13]),
                                              "Rend. médio mensal habitual - Trab principal - Superior completo ou equivalente" = as.numeric(df[10,14]),
                                              "cv_7" = as.numeric(df[10,15]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Sem instrução e menos de 1 ano de estudo" = as.numeric(df[10,16]),
                                              "cv_8" = as.numeric(df[10,17]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental incompleto ou equivalente" = as.numeric(df[10,18]),
                                              "cv_9" = as.numeric(df[10,19]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Fundamental completo ou equivalente" = as.numeric(df[10,20]),
                                              "cv_10" = as.numeric(df[10,21]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio incompleto ou equivalente" = as.numeric(df[10,22]),
                                              "cv_11" = as.numeric(df[10,23]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Médio completo ou equivalente" = as.numeric(df[10,24]),
                                              "cv_12" = as.numeric(df[10,25]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior incompleto ou equivalente" = as.numeric(df[10,26]),
                                              "cv_13" = as.numeric(df[10,27]),
                                              "Rend. médio mensal habitual - Todos os trabalhos - Superior completo ou equivalente" = as.numeric(df[10,28]),
                                              "cv_14" = as.numeric(df[10,29])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualrendm5438<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                      "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                      "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                      "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualrendm5438,file = "C:/Users/italo/Desktop/MICRODADOS/baseestratatualrendm5438.rds")

