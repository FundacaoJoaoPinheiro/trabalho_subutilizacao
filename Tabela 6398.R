################################################################################
##    LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG     ##
################################################################################

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

#Tabela 6398 - Pessoas de 14 anos ou mais de idade, por tipo de medida de subutilização da força de trabalho na semana de referência e sexo

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
                                   "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "VD4003", "VD4007", "VD4009", "VD4001", "V2009", "VD4001", "V2007", "V2010", "VD3004", "V4076", "VD4004"))) %>%
    
    update(
      #Criação das variáveis para população, conforme variável VD4007
      pia = as.numeric(V2009 >= 14) ,
      
      #Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)
      ocupada1a = 1 * (VD4002==2),
      ocupada1b = 1 * (VD4002==2 & V2007==1),
      ocupada1c = 1 * (VD4002==2 & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)
      ocupada2a = 1 * (VD4004==1),
      ocupada2b = 1 * (VD4004==1 & V2007==1),
      ocupada2c = 1 * (VD4004==1 & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)
      ocupada3a = 1 * (VD4003==1),
      ocupada3b = 1 * (VD4003==1 & V2007==1),
      ocupada3c = 1 * (VD4003==1 & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)
      ocupada4a = 1 * (VD4002==2 | VD4003==1),
      ocupada4b = 1 * ((VD4002==2 | VD4003==1) & V2007==1),
      ocupada4c = 1 * ((VD4002==2 | VD4003==1) & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)
      ocupada5a = 1 * (VD4002==2 | VD4004==1),
      ocupada5b = 1 * ((VD4002==2 | VD4004==1) & V2007==1),
      ocupada5c = 1 * ((VD4002==2 | VD4004==1) & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)
      ocupada6a = 1 * (VD4002==2 | VD4004==1 | VD4003==1),
      ocupada6b = 1 * ((VD4002==2 | VD4004==1 | VD4003==1) & V2007==1),
      ocupada6c = 1 * ((VD4002==2 | VD4004==1 | VD4003==1) & V2007==2),
      
      #Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)
      ocupada7a = 1 * (VD4002==1 | VD4002==2 | VD4003==1),
      ocupada7b = 1 * ((VD4002==1 | VD4002==2 | VD4003==1) & V2007==1),
      ocupada7c = 1 * ((VD4002==1 | VD4002==2 | VD4003==1) & V2007==2),

      regioes = case_when(
        Estrato %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~"01-Belo Horizonte",
        Estrato %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~"02-Entorno + Colar Metropolitano de BH",
        Estrato %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~"03-Sul de Minas",
        Estrato %in% c("3152011","3152012","3152013","3152021","3152022") ~"04-Triângulo Mineiro",
        Estrato %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~"05-Mata de Minas Gerais",
        Estrato %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~"06-Norte de MG + RIDE de Brasília em Minas",
        Estrato %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~"07-Vale do Rio Doce",
        Estrato %in% c("3156011","3156012","3156013","3156021","3156022") ~"08-Central",
        TRUE ~ "09 - Minas Gerais")
      # regioes
    )
  # Criando estimativas regionais (reescrevendo parte do código)
  
  ##
  estimativas_1 <- svyby(~ocupada1a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_1)[3]<-"cv_1"
  
  ##
  estimativas_2 <- svyby(~ocupada1b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_2)[3]<-"cv_2"
  
  ##
  estimativas_3 <- svyby(~ocupada1c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_3)[3]<-"cv_3"
  
  ######################
  ##
  estimativas_4 <- svyby(~ocupada2a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_4)[3]<-"cv_4"
  
  ##
  estimativas_5 <- svyby(~ocupada2b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_5)[3]<-"cv_5"
  
  ##
  estimativas_6 <- svyby(~ocupada2c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_6)[3]<-"cv_6"
  
  ######################
  ##
  estimativas_7 <- svyby(~ocupada3a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_7)[3]<-"cv_7"
  
  ##
  estimativas_8 <- svyby(~ocupada3b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_8)[3]<-"cv_8"
  
  ##
  estimativas_9 <- svyby(~ocupada3c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_9)[3]<-"cv_9"
  
  ######################
  ##
  estimativas_10 <- svyby(~ocupada4a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_10)[3]<-"cv_10"
  
  ##
  estimativas_11 <- svyby(~ocupada4b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_11)[3]<-"cv_11"
  
  ##
  estimativas_12 <- svyby(~ocupada4c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_12)[3]<-"cv_12"
  
  ######################
  ##
  estimativas_13 <- svyby(~ocupada5a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_13)[3]<-"cv_13"
  
  ##
  estimativas_14 <- svyby(~ocupada5b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_14)[3]<-"cv_14"
  
  ##
  estimativas_15 <- svyby(~ocupada5c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_15)[3]<-"cv_15"
  
  ######################
  ##
  estimativas_16 <- svyby(~ocupada6a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_16)[3]<-"cv_16"
  
  ##
  estimativas_17 <- svyby(~ocupada6b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_17)[3]<-"cv_17"
  
  ##
  estimativas_18 <- svyby(~ocupada6c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_18)[3]<-"cv_18"
  
  ######################
  ##
  estimativas_19 <- svyby(~ocupada7a, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_19)[3]<-"cv_19"
  
  ##
  estimativas_20 <- svyby(~ocupada7b, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_20)[3]<-"cv_20"
  
  ##
  estimativas_21 <- svyby(~ocupada7c, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_21)[3]<-"cv_21"
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_1<- svytotal(~ocupada1a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_2<- svytotal(~ocupada1b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_3<- svytotal(~ocupada1c, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_4<- svytotal(~ocupada2a, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_5<- svytotal(~ocupada2b, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_6<- svytotal(~ocupada2c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_7<- svytotal(~ocupada3a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_8<- svytotal(~ocupada3b, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_9<- svytotal(~ocupada3c, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_10<- svytotal(~ocupada4a, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_11<- svytotal(~ocupada4b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_12<- svytotal(~ocupada4c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_13<- svytotal(~ocupada5a, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_14<- svytotal(~ocupada5b, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_15<- svytotal(~ocupada5c, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_16<- svytotal(~ocupada6a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_17<- svytotal(~ocupada6b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_18<- svytotal(~ocupada6c, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_19<- svytotal(~ocupada7a, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_20<- svytotal(~ocupada7b, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_21<- svytotal(~ocupada7c, subset(pnadc,UF=="31"), na.rm = TRUE)
  
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "09 - Minas Gerais",
    
    ocupada1a = coef(total_1),
    cv_1 = round(cv(total_1)*100,1),
    
    ocupada1b = coef(total_2),
    cv_2 = round(cv(total_2)*100,1),

    ocupada1c = coef(total_3),
    cv_3 = round(cv(total_3)*100,1),
    
    
    ocupada2a = coef(total_4),
    cv_4 = round(cv(total_4)*100,1),
    
    ocupada2b = coef(total_5),
    cv_5 = round(cv(total_5)*100,1),
    
    ocupada2c = coef(total_6),
    cv_6 = round(cv(total_6)*100,1),
    
    
    ocupada3a = coef(total_7),
    cv_7 = round(cv(total_7)*100,1),
    
    ocupada3b = coef(total_8),
    cv_8 = round(cv(total_8)*100,1),
    
    ocupada3c = coef(total_9),
    cv_9 = round(cv(total_9)*100,1),
    
    
    ocupada4a = coef(total_10),
    cv_10 = round(cv(total_10)*100,1),
    
    ocupada4b = coef(total_11),
    cv_11 = round(cv(total_11)*100,1),
    
    ocupada4c = coef(total_12),
    cv_12 = round(cv(total_12)*100,1),
    
    
    ocupada5a = coef(total_13),
    cv_13 = round(cv(total_13)*100,1),
    
    ocupada5b = coef(total_14),
    cv_14 = round(cv(total_14)*100,1),
    
    ocupada5c = coef(total_15),
    cv_15 = round(cv(total_15)*100,1),
    
    
    ocupada6a = coef(total_16),
    cv_16 = round(cv(total_16)*100,1),
    
    ocupada6b = coef(total_17),
    cv_17 = round(cv(total_17)*100,1),
    
    ocupada6c = coef(total_18),
    cv_18 = round(cv(total_18)*100,1),
    
    
    ocupada7a = coef(total_19),
    cv_19 = round(cv(total_19)*100,1),
    
    ocupada7b = coef(total_20),
    cv_20 = round(cv(total_20)*100,1),
    
    ocupada7c = coef(total_21),
    cv_21 = round(cv(total_21)*100,1),
    

    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
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
    left_join(estimativas_15) %>% 
    left_join(estimativas_16) %>%
    left_join(estimativas_17) %>%
    left_join(estimativas_18) %>% 
    left_join(estimativas_19) %>% 
    left_join(estimativas_20) %>%
    left_join(estimativas_21) %>%

    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, estimativas_12, estimativas_13, estimativas_14, estimativas_15, estimativas_16, estimativas_17, estimativas_18, estimativas_19, estimativas_20, estimativas_21, pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas, paste0("C:/Users/italo/Desktop/trimestral/estimativas/resultados_0",mesano,".RDS"))
  paste("Concluído:",mesano)
  
}
## Fim da função

## Atualizando lista até 2023:
## Continuando a formatação por colunas

lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
           022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
           032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
           042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023,042024)

sapply(lista, function(i) calcula_ocup_desocup(i))


#Calcular os coeficientes de variação 
################################################################################
##          CRIANDO OS DFS PARA OS 10 ESTRATOS - 2012 A 2024                  ##
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
                                     13,26,39,52)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[9,44]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[9,2]),
                                         "cv" = as.numeric(df[9,3]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[9,4]),
                                         "cv" = as.numeric(df[9,5]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[9,6]),
                                         "cv" = as.numeric(df[9,7]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[9,8]),
                                         "cv" = as.numeric(df[9,9]),
                                         "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[9,10]),
                                         "cv" = as.numeric(df[9,11]),
                                         "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[9,12]),
                                         "cv" = as.numeric(df[9,13]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[9,14]),
                                         "cv" = as.numeric(df[9,15]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[9,16]),
                                         "cv" = as.numeric(df[9,17]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[9,18]),
                                         "cv" = as.numeric(df[9,19]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[9,20]),
                                         "cv" = as.numeric(df[9,21]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[9,22]),
                                         "cv" = as.numeric(df[9,23]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[9,24]),
                                         "cv" = as.numeric(df[9,25]),
                                      
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[9,26]),
                                         "cv" = as.numeric(df[9,27]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[9,28]),
                                         "cv" = as.numeric(df[9,29]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[9,30]),
                                         "cv" = as.numeric(df[9,31]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[9,32]),
                                         "cv" = as.numeric(df[9,33]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[9,34]),
                                         "cv" = as.numeric(df[9,35]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[9,36]),
                                         "cv" = as.numeric(df[9,37]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[9,38]),
                                         "cv" = as.numeric(df[9,39]),
                                         "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[9,40]),
                                         "cv" = as.numeric(df[9,41]),
                                         "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[9,42]),
                                         "cv" = as.numeric(df[9,43])
                                         
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
                                     13,26,39,52)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1,44]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[1,4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[1,6]),
                                         "cv" = as.numeric(df[1,7]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[1,8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[1,10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[1,12]),
                                         "cv" = as.numeric(df[1,13]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[1,14]),
                                         "cv" = as.numeric(df[1,15]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[1,16]),
                                         "cv" = as.numeric(df[1,17]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[1,18]),
                                         "cv" = as.numeric(df[1,19]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[1,20]),
                                         "cv" = as.numeric(df[1,21]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[1,22]),
                                         "cv" = as.numeric(df[1,23]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[1,24]),
                                         "cv" = as.numeric(df[1,25]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[1,26]),
                                         "cv" = as.numeric(df[1,27]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[1,28]),
                                         "cv" = as.numeric(df[1,29]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[1,30]),
                                         "cv" = as.numeric(df[1,31]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[1,32]),
                                         "cv" = as.numeric(df[1,33]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[1,34]),
                                         "cv" = as.numeric(df[1,35]),
                                         "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[1,36]),
                                         "cv" = as.numeric(df[1,37]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[1,38]),
                                         "cv" = as.numeric(df[1,39]),
                                         "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[1,40]),
                                         "cv" = as.numeric(df[1,41]),
                                         "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[1,42]),
                                         "cv" = as.numeric(df[1,43])
                                       )
                                     }))

#View(bh)

######################################
### Entorno Metropolitano de BH (linha: 2)

entornobhcolar<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
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
                                            13,26,39,52)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2,44]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[2,2]),
                                                "cv" = as.numeric(df[2,3]),
                                                "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[2,4]),
                                                "cv" = as.numeric(df[2,5]),
                                                "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[2,6]),
                                                "cv" = as.numeric(df[2,7]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[2,8]),
                                                "cv" = as.numeric(df[2,9]),
                                                "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[2,10]),
                                                "cv" = as.numeric(df[2,11]),
                                                "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[2,12]),
                                                "cv" = as.numeric(df[2,13]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[2,14]),
                                                "cv" = as.numeric(df[2,15]),
                                                "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[2,16]),
                                                "cv" = as.numeric(df[2,17]),
                                                "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[2,18]),
                                                "cv" = as.numeric(df[2,19]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[2,20]),
                                                "cv" = as.numeric(df[2,21]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[2,22]),
                                                "cv" = as.numeric(df[2,23]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[2,24]),
                                                "cv" = as.numeric(df[2,25]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[2,26]),
                                                "cv" = as.numeric(df[2,27]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[2,28]),
                                                "cv" = as.numeric(df[2,29]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[2,30]),
                                                "cv" = as.numeric(df[2,31]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[2,32]),
                                                "cv" = as.numeric(df[2,33]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[2,34]),
                                                "cv" = as.numeric(df[2,35]),
                                                "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[2,36]),
                                                "cv" = as.numeric(df[2,37]),
                                                
                                                "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[2,38]),
                                                "cv" = as.numeric(df[2,39]),
                                                "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[2,40]),
                                                "cv" = as.numeric(df[2,41]),
                                                "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[2,42]),
                                                "cv" = as.numeric(df[2,43])
                                              )
                                            }))

#View(entornobh)

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
                                        13,26,39,52)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[3,44]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[3,2]),
                                            "cv" = as.numeric(df[3,3]),
                                            "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[3,4]),
                                            "cv" = as.numeric(df[3,5]),
                                            "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[3,6]),
                                            "cv" = as.numeric(df[3,7]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[3,8]),
                                            "cv" = as.numeric(df[3,9]),
                                            "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[3,10]),
                                            "cv" = as.numeric(df[3,11]),
                                            "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[3,12]),
                                            "cv" = as.numeric(df[3,13]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[3,14]),
                                            "cv" = as.numeric(df[3,15]),
                                            "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[3,16]),
                                            "cv" = as.numeric(df[3,17]),
                                            "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[3,18]),
                                            "cv" = as.numeric(df[3,19]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[3,20]),
                                            "cv" = as.numeric(df[3,21]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[3,22]),
                                            "cv" = as.numeric(df[3,23]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[3,24]),
                                            "cv" = as.numeric(df[3,25]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[3,26]),
                                            "cv" = as.numeric(df[3,27]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[3,28]),
                                            "cv" = as.numeric(df[3,29]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[3,30]),
                                            "cv" = as.numeric(df[3,31]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[3,32]),
                                            "cv" = as.numeric(df[3,33]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[3,34]),
                                            "cv" = as.numeric(df[3,35]),
                                            "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[3,36]),
                                            "cv" = as.numeric(df[3,37]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[3,38]),
                                            "cv" = as.numeric(df[3,39]),
                                            "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[3,40]),
                                            "cv" = as.numeric(df[3,41]),
                                            "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[3,42]),
                                            "cv" = as.numeric(df[3,43])
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
                                       13,26,39,52)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4,44]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[4,4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[4,6]),
                                           "cv" = as.numeric(df[4,7]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[4,8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[4,10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[4,12]),
                                           "cv" = as.numeric(df[4,13]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[4,14]),
                                           "cv" = as.numeric(df[4,15]),
                                           "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[4,16]),
                                           "cv" = as.numeric(df[4,17]),
                                           "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[4,18]),
                                           "cv" = as.numeric(df[4,19]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[4,20]),
                                           "cv" = as.numeric(df[4,21]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[4,22]),
                                           "cv" = as.numeric(df[4,23]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[4,24]),
                                           "cv" = as.numeric(df[4,25]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[4,26]),
                                           "cv" = as.numeric(df[4,27]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[4,28]),
                                           "cv" = as.numeric(df[4,29]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[4,30]),
                                           "cv" = as.numeric(df[4,31]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[4,32]),
                                           "cv" = as.numeric(df[4,33]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[4,34]),
                                           "cv" = as.numeric(df[4,35]),
                                           "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[4,36]),
                                           "cv" = as.numeric(df[4,37]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[4,38]),
                                           "cv" = as.numeric(df[4,39]),
                                           "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[4,40]),
                                           "cv" = as.numeric(df[4,41]),
                                           "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[4,42]),
                                           "cv" = as.numeric(df[4,43])
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
                                           13,26,39,52)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[5,44]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[5,2]),
                                               "cv" = as.numeric(df[5,3]),
                                               "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[5,4]),
                                               "cv" = as.numeric(df[5,5]),
                                               "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[5,6]),
                                               "cv" = as.numeric(df[5,7]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[5,8]),
                                               "cv" = as.numeric(df[5,9]),
                                               "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[5,10]),
                                               "cv" = as.numeric(df[5,11]),
                                               "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[5,12]),
                                               "cv" = as.numeric(df[5,13]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[5,14]),
                                               "cv" = as.numeric(df[5,15]),
                                               "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[5,16]),
                                               "cv" = as.numeric(df[5,17]),
                                               "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[5,18]),
                                               "cv" = as.numeric(df[5,19]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[5,20]),
                                               "cv" = as.numeric(df[5,21]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[5,22]),
                                               "cv" = as.numeric(df[5,23]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[5,24]),
                                               "cv" = as.numeric(df[5,25]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[5,26]),
                                               "cv" = as.numeric(df[5,27]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[5,28]),
                                               "cv" = as.numeric(df[5,29]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[5,30]),
                                               "cv" = as.numeric(df[5,31]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[5,32]),
                                               "cv" = as.numeric(df[5,33]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[5,34]),
                                               "cv" = as.numeric(df[5,35]),
                                               "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[5,36]),
                                               "cv" = as.numeric(df[5,37]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[5,38]),
                                               "cv" = as.numeric(df[5,39]),
                                               "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[5,40]),
                                               "cv" = as.numeric(df[5,41]),
                                               "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[5,42]),
                                               "cv" = as.numeric(df[5,43])
                                             )
                                           }))

#View(zonamata)

############################################################
### Norte de Minas (Linha: 8)

nortemgride<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
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
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[6,44]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[6,2]),
                                              "cv" = as.numeric(df[6,3]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[6,4]),
                                              "cv" = as.numeric(df[6,5]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[6,6]),
                                              "cv" = as.numeric(df[6,7]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[6,8]),
                                              "cv" = as.numeric(df[6,9]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[6,10]),
                                              "cv" = as.numeric(df[6,11]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[6,12]),
                                              "cv" = as.numeric(df[6,13]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[6,14]),
                                              "cv" = as.numeric(df[6,15]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[6,16]),
                                              "cv" = as.numeric(df[6,17]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[6,18]),
                                              "cv" = as.numeric(df[6,19]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[6,20]),
                                              "cv" = as.numeric(df[6,21]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[6,22]),
                                              "cv" = as.numeric(df[6,23]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[6,24]),
                                              "cv" = as.numeric(df[6,25]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[6,26]),
                                              "cv" = as.numeric(df[6,27]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[6,28]),
                                              "cv" = as.numeric(df[6,29]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[6,30]),
                                              "cv" = as.numeric(df[6,31]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[6,32]),
                                              "cv" = as.numeric(df[6,33]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[6,34]),
                                              "cv" = as.numeric(df[6,35]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[6,36]),
                                              "cv" = as.numeric(df[6,37]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[6,38]),
                                              "cv" = as.numeric(df[6,39]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[6,40]),
                                              "cv" = as.numeric(df[6,41]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[6,42]),
                                              "cv" = as.numeric(df[6,43])
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
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[7,44]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[7,2]),
                                              "cv" = as.numeric(df[7,3]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[7,4]),
                                              "cv" = as.numeric(df[7,5]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[7,6]),
                                              "cv" = as.numeric(df[7,7]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[7,8]),
                                              "cv" = as.numeric(df[7,9]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[7,10]),
                                              "cv" = as.numeric(df[7,11]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[7,12]),
                                              "cv" = as.numeric(df[7,13]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[7,14]),
                                              "cv" = as.numeric(df[7,15]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[7,16]),
                                              "cv" = as.numeric(df[7,17]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[7,18]),
                                              "cv" = as.numeric(df[7,19]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[7,20]),
                                              "cv" = as.numeric(df[7,21]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[7,22]),
                                              "cv" = as.numeric(df[7,23]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[7,24]),
                                              "cv" = as.numeric(df[7,25]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[7,26]),
                                              "cv" = as.numeric(df[7,27]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[7,28]),
                                              "cv" = as.numeric(df[7,29]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[7,30]),
                                              "cv" = as.numeric(df[7,31]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[7,32]),
                                              "cv" = as.numeric(df[7,33]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[7,34]),
                                              "cv" = as.numeric(df[7,35]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[7,36]),
                                              "cv" = as.numeric(df[7,37]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[7,38]),
                                              "cv" = as.numeric(df[7,39]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[7,40]),
                                              "cv" = as.numeric(df[7,41]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[7,42]),
                                              "cv" = as.numeric(df[7,43])
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
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8,44]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Homens" = as.numeric(df[8,4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - Mulheres" = as.numeric(df[8,6]),
                                              "cv" = as.numeric(df[8,7]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[8,8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[8,10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Pessoas de 14 anos ou mais de idade, subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[8,12]),
                                              "cv" = as.numeric(df[8,13]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas)" = as.numeric(df[8,14]),
                                              "cv" = as.numeric(df[8,15]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[8,16]),
                                              "cv" = as.numeric(df[8,17]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[8,18]),
                                              "cv" = as.numeric(df[8,19]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[8,20]),
                                              "cv" = as.numeric(df[8,21]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[8,22]),
                                              "cv" = as.numeric(df[8,23]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[8,24]),
                                              "cv" = as.numeric(df[8,25]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas)" = as.numeric(df[8,26]),
                                              "cv" = as.numeric(df[8,27]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Homens" = as.numeric(df[8,28]),
                                              "cv" = as.numeric(df[8,29]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas (Mil pessoas) - Mulheres" = as.numeric(df[8,30]),
                                              "cv" = as.numeric(df[8,31]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas)" = as.numeric(df[8,32]),
                                              "cv" = as.numeric(df[8,33]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Homens" = as.numeric(df[8,34]),
                                              "cv" = as.numeric(df[8,35]),
                                              "Pessoas de 14 anos ou mais de idade desocupadas ou subocupadas por insuficiência de horas trabalhadas ou na força de trabalho potencial (Mil pessoas) - Mulheres" = as.numeric(df[8,36]),
                                              "cv" = as.numeric(df[8,37]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas)" = as.numeric(df[8,38]),
                                              "cv" = as.numeric(df[8,39]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Homens" = as.numeric(df[8,40]),
                                              "cv" = as.numeric(df[8,41]),
                                              "Pessoas de 14 anos ou mais de idade na força de trabalho ampliada (Mil pessoas) - Mulheres" = as.numeric(df[8,42]),
                                              "cv" = as.numeric(df[8,43])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualV6398<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano + Colar de BH"=entornobhcolar,
                           "03-Sul de Minas"=sulmg, "04-Triângulo Mineiro"=trng,
                           "05-Mata de Minas Gerais"=zonamata, "06-Norte de Minas + RIDE de Brasilia em MG"=nortemgride, "07-Vale do Rio Doce"=riodoce,
                           "08-Central"=central, "09 - Minas Gerais"=mg)

saveRDS(baseestratatualV6398,file = "C:/Users/italo/Desktop/trimestral/baseestratatualV6398.rds")

