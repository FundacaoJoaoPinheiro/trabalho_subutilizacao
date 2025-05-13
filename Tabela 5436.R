################################################################################
##  LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG       ##
################################################################################

#Tabela 5436 - Rendimento médio mensal real das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente e efetivamente recebidos no trabalho principal e em todos os trabalhos, por sexo

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)


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
  pnadc <- read_pnadc(paste0("C:/Users/italo/Desktop/trimestral/txt/PNADC_0",mesano,".txt"), 
                      "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "V2009", "VD4001", "V2007", "V2010", "VD3004", "V4076", "VD4015", "VD4019","VD4020", "VD4016", "VD4017", "VD4007")) 
  pnadc <- pnadc_deflator(data_pnadc=pnadc, deflator.file="C:/Users/italo/Desktop/trimestral/deflator_PNADC_trimestral.xls")
  pnadc <- pnadc_design(data_pnadc=pnadc)  %>%
    
    update(
      #Criando as variáveis de renda, conforme variável V4007(posição na ocupação principal)
      pia = as.numeric(V2009 >= 14),
      
      #Trabalho principal - VD4016 E VD4017
      VD4016rph = as.numeric(VD4016 * Habitual),
      
      VD4017rpe = as.numeric(VD4017 * Efetivo),
      
      #Todos os trabalhos - VD4019 E VD4020
      VD4019rth = as.numeric(VD4019 * Habitual),
      
      VD4020rte = as.numeric(VD4020 * Efetivo),
      
      #Variaveis
      
      #Habitual Principal
      VD4016a_hhp = ifelse(pia==1 & VD4002==1 & V2007 == 1, VD4016rph, NA),
      VD4016b_mhp = ifelse(pia==1 & VD4002==1 & V2007 == 2, VD4016rph, NA),
      
      #Efetivo principal
      VD4017a_hep = ifelse(pia==1 & VD4002==1 & V2007 == 1, VD4017rpe, NA),
      VD4017b_mep = ifelse(pia==1 & VD4002==1 & V2007 == 2, VD4017rpe, NA),
      
      #Habitual Todos
      VD4019a_hht = ifelse(pia==1 & VD4002==1 & V2007 == 1, VD4019rth, NA),
      VD4019b_mht = ifelse(pia==1 & VD4002==1 & V2007 == 2, VD4019rth, NA),
      
      #Efetivo todos
      VD4020a_het = ifelse(pia==1 & VD4002==1 & V2007 == 1, VD4020rte, NA),
      VD4020b_met = ifelse(pia==1 & VD4002==1 & V2007 == 2, VD4020rte, NA),

      
      #Totais
      one1 = ifelse(pia==1 & VD4002==1, VD4016rph, NA),
      one2 = ifelse(pia==1 & VD4002==1, VD4017rpe, NA),
      one3 = ifelse(pia==1 & VD4002==1, VD4019rth, NA),
      one4 = ifelse(pia==1 & VD4002==1, VD4020rte, NA),
    
      regioes = case_when(
        Estrato %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~"01-Belo Horizonte",
        Estrato %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~"02-Entorno + Colar Metropolitano de BH",
        Estrato %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~"03-Sul de Minas",
        Estrato %in% c("3152011","3152012","3152013","3152021","3152022") ~"04-Triângulo Mineiro",
        Estrato %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~"05-Mata de Minas Gerais",
        Estrato %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~"06-Norte de MG + RIDE de Brasília em Minas",
        Estrato %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~"07-Vale do Rio Doce",
        Estrato %in% c("3156011","3156012","3156013","3156021","3156022") ~"08-Central",
        TRUE ~ "11 - Minas Gerais")
      # regioes
    )
  # Criando estimativas regionais (reescrevendo parte do código)
  
  ##Total Geral - Habitual Principal
  estimativas_t1 <- svyby(~one1, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t1)[3]<-"cv_t1"
  
  estimativas_1 <- svyby(~VD4016a_hhp, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_1)[3]<-"cv_1"
  
  estimativas_2 <- svyby(~VD4016b_mhp, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_2)[3]<-"cv_2"
  
  
  ##Total Geral - Efetivo Principal
  estimativas_t2 <- svyby(~one2, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t2)[3]<-"cv_t2"
  
  estimativas_3 <- svyby(~VD4017a_hep, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_3)[3]<-"cv_3"
  
  estimativas_4 <- svyby(~VD4017b_mep, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_4)[3]<-"cv_4"
  
  
  ##Total Geral - Habitual Todos
  estimativas_t3 <- svyby(~one3, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t3)[3]<-"cv_t3"
  
  estimativas_5 <- svyby(~VD4019a_hht, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_5)[3]<-"cv_5"
  
  estimativas_6 <- svyby(~VD4019b_mht, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_6)[3]<-"cv_6"
  
  
  ##Total Geral - Efetivo Todos
  estimativas_t4 <- svyby(~one4, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t4)[3]<-"cv_t4"
  
  estimativas_7 <- svyby(~VD4020a_het, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_7)[3]<-"cv_7"
  
  estimativas_8 <- svyby(~VD4020b_met, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_8)[3]<-"cv_8"
  
  
  
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_t1<- svymean(~one1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t2<- svymean(~one2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t3<- svymean(~one3, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t4<- svymean(~one4, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  total_ocupada1<- svymean(~VD4016a_hhp, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada2<- svymean(~VD4016b_mhp, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada3<- svymean(~VD4017a_hep, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada4<- svymean(~VD4017b_mep, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada5<- svymean(~VD4019a_hht, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada6<- svymean(~VD4019b_mht, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada7<- svymean(~VD4020a_het, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada8<- svymean(~VD4020b_met, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "09 - Minas Gerais",
    
    one1 = coef(total_t1),
    cv_t1 = round(cv(total_t1)*100,1),
    VD4016a_hhp = coef(total_ocupada1),
    cv_1 = round(cv(total_ocupada1)*100,1),
    VD4016b_mhp = coef(total_ocupada2),
    cv_2 = round(cv(total_ocupada2)*100,1),
    
    
    one2 = coef(total_t2),
    cv_t2 = round(cv(total_t2)*100,1),
    VD4017a_hep = coef(total_ocupada3),
    cv_3 = round(cv(total_ocupada3)*100,1),
    VD4017b_mep = coef(total_ocupada4),
    cv_4 = round(cv(total_ocupada4)*100,1),
    
    one3 = coef(total_t3),
    cv_t3 = round(cv(total_t3)*100,1),
    VD4019a_hht = coef(total_ocupada5),
    cv_5 = round(cv(total_ocupada5)*100,1),
    VD4019b_mht = coef(total_ocupada6),
    cv_6 = round(cv(total_ocupada6)*100,1),
    
    one4 = coef(total_t4),
    cv_t4 = round(cv(total_t4)*100,1),
    VD4020a_het = coef(total_ocupada7),
    cv_7 = round(cv(total_ocupada7)*100,1),
    VD4020b_met = coef(total_ocupada8),
    cv_8 = round(cv(total_ocupada8)*100,1),
    
    
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas_t1 %>% 
    left_join(estimativas_1) %>% 
    left_join(estimativas_2) %>% 
    left_join(estimativas_t2) %>%
    left_join(estimativas_3) %>% 
    left_join(estimativas_4) %>% 
    left_join(estimativas_t3) %>%
    left_join(estimativas_5) %>% 
    left_join(estimativas_6) %>% 
    left_join(estimativas_t4) %>%
    left_join(estimativas_7) %>% 
    left_join(estimativas_8) %>%
    
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_t1, estimativas_t2, estimativas_t3, estimativas_t4, estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas,paste0("C:/Users/italo/Desktop/trimestral/estimativas/resultados_0",mesano,".RDS"))
  paste("Concluído:",mesano)
  
}
## Fim da função

## Atualizando lista até 2024:
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
                                         "Período" = as.character(df[9, 26]),
                                         
                                         "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[9,2]),
                                         "cv_t1" = as.numeric(df[9,3]),
                                         "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[9,4]),
                                         "cv_1" = as.numeric(df[9,5]),
                                         "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[9,6]),
                                         "cv_2" = as.numeric(df[9,7]),
                                         
                                         "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[9,8]),
                                         "cv_t2" = as.numeric(df[9,9]),
                                         "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[9,10]),
                                         "cv_3" = as.numeric(df[9,11]),
                                         "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[9,12]),
                                         "cv_4" = as.numeric(df[9,13]),
                                         
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[9,14]),
                                         "cv_t3" = as.numeric(df[9,15]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[9,16]),
                                         "cv_5" = as.numeric(df[9,17]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[9,18]),
                                         "cv_6" = as.numeric(df[9,19]),
                                         
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[9,20]),
                                         "cv_t4" = as.numeric(df[9,21]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[9,22]),
                                         "cv_7" = as.numeric(df[9,23]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[9,24]),
                                         "cv_8" = as.numeric(df[9,25])
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
                                         "Período" = as.character(df[1, 26]),
                                         
                                         "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[1,2]),
                                         "cv_t1" = as.numeric(df[1,3]),
                                         "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[1,4]),
                                         "cv_1" = as.numeric(df[1,5]),
                                         "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[1,6]),
                                         "cv_2" = as.numeric(df[1,7]),
                                         
                                         "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[1,8]),
                                         "cv_t2" = as.numeric(df[1,9]),
                                         "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[1,10]),
                                         "cv_3" = as.numeric(df[1,11]),
                                         "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[1,12]),
                                         "cv_4" = as.numeric(df[1,13]),
                                         
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[1,14]),
                                         "cv_t3" = as.numeric(df[1,15]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[1,16]),
                                         "cv_5" = as.numeric(df[1,17]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[1,18]),
                                         "cv_6" = as.numeric(df[1,19]),
                                         
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[1,20]),
                                         "cv_t4" = as.numeric(df[1,21]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[1,22]),
                                         "cv_7" = as.numeric(df[1,23]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[1,24]),
                                         "cv_8" = as.numeric(df[1,25])
                                         
          
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
                                                "Período" = as.character(df[2, 26]),
                                                
                                                "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[2,2]),
                                                "cv_t1" = as.numeric(df[2,3]),
                                                "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[2,4]),
                                                "cv_1" = as.numeric(df[2,5]),
                                                "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[2,6]),
                                                "cv_2" = as.numeric(df[2,7]),
                                                
                                                "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[2,8]),
                                                "cv_t2" = as.numeric(df[2,9]),
                                                "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[2,10]),
                                                "cv_3" = as.numeric(df[2,11]),
                                                "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[2,12]),
                                                "cv_4" = as.numeric(df[2,13]),
                                                
                                                "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[2,14]),
                                                "cv_t3" = as.numeric(df[2,15]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[2,16]),
                                                "cv_5" = as.numeric(df[2,17]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[2,18]),
                                                "cv_6" = as.numeric(df[2,19]),
                                                
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[2,20]),
                                                "cv_t4" = as.numeric(df[2,21]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[2,22]),
                                                "cv_7" = as.numeric(df[2,23]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[2,24]),
                                                "cv_8" = as.numeric(df[2,25])
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
                                            "Período" = as.character(df[3, 26]),
                                            
                                            "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[3,2]),
                                            "cv_t1" = as.numeric(df[3,3]),
                                            "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[3,4]),
                                            "cv_1" = as.numeric(df[3,5]),
                                            "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[3,6]),
                                            "cv_2" = as.numeric(df[3,7]),
                                            
                                            "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[3,8]),
                                            "cv_t2" = as.numeric(df[3,9]),
                                            "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[3,10]),
                                            "cv_3" = as.numeric(df[3,11]),
                                            "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[3,12]),
                                            "cv_4" = as.numeric(df[3,13]),
                                            
                                            "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[3,14]),
                                            "cv_t3" = as.numeric(df[3,15]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[3,16]),
                                            "cv_5" = as.numeric(df[3,17]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[3,18]),
                                            "cv_6" = as.numeric(df[3,19]),
                                            
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[3,20]),
                                            "cv_t4" = as.numeric(df[3,21]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[3,22]),
                                            "cv_7" = as.numeric(df[3,23]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[3,24]),
                                            "cv_8" = as.numeric(df[3,25])
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
                                           "Período" = as.character(df[4, 26]),
                                           
                                           "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[4,2]),
                                           "cv_t1" = as.numeric(df[4,3]),
                                           "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[4,4]),
                                           "cv_1" = as.numeric(df[4,5]),
                                           "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[4,6]),
                                           "cv_2" = as.numeric(df[4,7]),
                                           
                                           "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[4,8]),
                                           "cv_t2" = as.numeric(df[4,9]),
                                           "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[4,10]),
                                           "cv_3" = as.numeric(df[4,11]),
                                           "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[4,12]),
                                           "cv_4" = as.numeric(df[4,13]),
                                           
                                           "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[4,14]),
                                           "cv_t3" = as.numeric(df[4,15]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[4,16]),
                                           "cv_5" = as.numeric(df[4,17]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[4,18]),
                                           "cv_6" = as.numeric(df[4,19]),
                                           
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[4,20]),
                                           "cv_t4" = as.numeric(df[4,21]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[4,22]),
                                           "cv_7" = as.numeric(df[4,23]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[4,24]),
                                           "cv_8" = as.numeric(df[4,25])
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
                                               "Período" = as.character(df[5, 26]),
                                               
                                               "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[5,2]),
                                               "cv_t1" = as.numeric(df[5,3]),
                                               "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[5,4]),
                                               "cv_1" = as.numeric(df[5,5]),
                                               "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[5,6]),
                                               "cv_2" = as.numeric(df[5,7]),
                                               
                                               "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[5,8]),
                                               "cv_t2" = as.numeric(df[5,9]),
                                               "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[5,10]),
                                               "cv_3" = as.numeric(df[5,11]),
                                               "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[5,12]),
                                               "cv_4" = as.numeric(df[5,13]),
                                               
                                               "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[5,14]),
                                               "cv_t3" = as.numeric(df[5,15]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[5,16]),
                                               "cv_5" = as.numeric(df[5,17]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[5,18]),
                                               "cv_6" = as.numeric(df[5,19]),
                                               
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[5,20]),
                                               "cv_t4" = as.numeric(df[5,21]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[5,22]),
                                               "cv_7" = as.numeric(df[5,23]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[5,24]),
                                               "cv_8" = as.numeric(df[5,25])
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
                                              "Período" = as.character(df[6, 26]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[6,2]),
                                              "cv_t1" = as.numeric(df[6,3]),
                                              "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[6,4]),
                                              "cv_1" = as.numeric(df[6,5]),
                                              "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[6,6]),
                                              "cv_2" = as.numeric(df[6,7]),
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[6,8]),
                                              "cv_t2" = as.numeric(df[6,9]),
                                              "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[6,10]),
                                              "cv_3" = as.numeric(df[6,11]),
                                              "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[6,12]),
                                              "cv_4" = as.numeric(df[6,13]),
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[6,14]),
                                              "cv_t3" = as.numeric(df[6,15]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[6,16]),
                                              "cv_5" = as.numeric(df[6,17]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[6,18]),
                                              "cv_6" = as.numeric(df[6,19]),
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[6,20]),
                                              "cv_t4" = as.numeric(df[6,21]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[6,22]),
                                              "cv_7" = as.numeric(df[6,23]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[6,24]),
                                              "cv_8" = as.numeric(df[6,25])
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
                                              "Período" = as.character(df[7, 26]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[7,2]),
                                              "cv_t1" = as.numeric(df[7,3]),
                                              "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[7,4]),
                                              "cv_1" = as.numeric(df[7,5]),
                                              "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[7,6]),
                                              "cv_2" = as.numeric(df[7,7]),
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[7,8]),
                                              "cv_t2" = as.numeric(df[7,9]),
                                              "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[7,10]),
                                              "cv_3" = as.numeric(df[7,11]),
                                              "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[7,12]),
                                              "cv_4" = as.numeric(df[7,13]),
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[7,14]),
                                              "cv_t3" = as.numeric(df[7,15]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[7,16]),
                                              "cv_5" = as.numeric(df[7,17]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[7,18]),
                                              "cv_6" = as.numeric(df[7,19]),
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[7,20]),
                                              "cv_t4" = as.numeric(df[7,21]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[7,22]),
                                              "cv_7" = as.numeric(df[7,23]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[7,24]),
                                              "cv_8" = as.numeric(df[7,25])
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
                                              "Período" = as.character(df[8, 26]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[8,2]),
                                              "cv_t1" = as.numeric(df[8,3]),
                                              "Rend. médio mensal habitual - Trab principal - Homem " = as.numeric(df[8,4]),
                                              "cv_1" = as.numeric(df[8,5]),
                                              "Rend. médio mensal habitual - Trab principal - Mulher" = as.numeric(df[8,6]),
                                              "cv_2" = as.numeric(df[8,7]),
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[8,8]),
                                              "cv_t2" = as.numeric(df[8,9]),
                                              "Rend. médio mensal efetivo - Trab principal - Homem " = as.numeric(df[8,10]),
                                              "cv_3" = as.numeric(df[8,11]),
                                              "Rend. médio mensal efetivo - Trab principal - Mulher" = as.numeric(df[8,12]),
                                              "cv_4" = as.numeric(df[8,13]),
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[8,14]),
                                              "cv_t3" = as.numeric(df[8,15]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Homem " = as.numeric(df[8,16]),
                                              "cv_5" = as.numeric(df[8,17]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Mulher" = as.numeric(df[8,18]),
                                              "cv_6" = as.numeric(df[8,19]),
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[8,20]),
                                              "cv_t4" = as.numeric(df[8,21]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Homem " = as.numeric(df[8,22]),
                                              "cv_7" = as.numeric(df[8,23]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Mulher" = as.numeric(df[8,24]),
                                              "cv_8" = as.numeric(df[8,25])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualrendm5436<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano + Colar de BH"=entornobhcolar,
                               "03-Sul de Minas"=sulmg, "04-Triângulo Mineiro"=trng,
                               "05-Mata de Minas Gerais"=zonamata, "06-Norte de Minas + RIDE de Brasilia em MG"=nortemgride, "07-Vale do Rio Doce"=riodoce,
                               "08-Central"=central, "09 - Minas Gerais"=mg)

saveRDS(baseestratatualrendm5436,file = "C:/Users/italo/Desktop/trimestral/baseestratatualrendm5436.rds")

