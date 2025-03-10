################################################################################
##  LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG       ##
################################################################################

#Tabela 5437 - Rendimento médio mensal real das pessoas de 14 anos ou mais de idade ocupadas na semana de referência com rendimento de trabalho, habitualmente e efetivamente recebidos no trabalho principal e em todos os trabalhos, por grupo de idade
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
      VD4016a = ifelse(pia==1 & VD4002==1 & (V2009 >= 14 & V2009 <= 17), VD4016rph, NA),
      VD4016b = ifelse(pia==1 & VD4002==1 & (V2009 >= 18 & V2009 <= 24), VD4016rph, NA),
      VD4016c = ifelse(pia==1 & VD4002==1 & (V2009 >= 25 & V2009 <= 39), VD4016rph, NA),
      VD4016d = ifelse(pia==1 & VD4002==1 & (V2009 >= 40 & V2009 <= 59), VD4016rph, NA),
      VD4016e = ifelse(pia==1 & VD4002==1 & (V2009 >= 60), VD4016rph, NA),
      
      #Efetivo principal
      VD4017a = ifelse(pia==1 & VD4002==1 & (V2009 >= 14 & V2009 <= 17), VD4017rpe, NA),
      VD4017b = ifelse(pia==1 & VD4002==1 & (V2009 >= 18 & V2009 <= 24), VD4017rpe, NA),
      VD4017c = ifelse(pia==1 & VD4002==1 & (V2009 >= 25 & V2009 <= 39), VD4017rpe, NA),
      VD4017d = ifelse(pia==1 & VD4002==1 & (V2009 >= 40 & V2009 <= 59), VD4017rpe, NA),
      VD4017e = ifelse(pia==1 & VD4002==1 & (V2009 >= 60), VD4017rpe, NA),
      
      
      #Habitual Todos
      VD4019a = ifelse(pia==1 & VD4002==1 & (V2009 >= 14 & V2009 <= 17), VD4019rth, NA),
      VD4019b = ifelse(pia==1 & VD4002==1 & (V2009 >= 18 & V2009 <= 24), VD4019rth, NA),
      VD4019c = ifelse(pia==1 & VD4002==1 & (V2009 >= 25 & V2009 <= 39), VD4019rth, NA),
      VD4019d = ifelse(pia==1 & VD4002==1 & (V2009 >= 40 & V2009 <= 59), VD4019rth, NA),
      VD4019e = ifelse(pia==1 & VD4002==1 & (V2009 >= 60), VD4019rth, NA),
      
      #Efetivo todos
      VD4020a = ifelse(pia==1 & VD4002==1 & (V2009 >= 14 & V2009 <= 17), VD4020rte, NA),
      VD4020b = ifelse(pia==1 & VD4002==1 & (V2009 >= 18 & V2009 <= 24), VD4020rte, NA),
      VD4020c = ifelse(pia==1 & VD4002==1 & (V2009 >= 25 & V2009 <= 39), VD4020rte, NA),
      VD4020d = ifelse(pia==1 & VD4002==1 & (V2009 >= 40 & V2009 <= 59), VD4020rte, NA),
      VD4020e = ifelse(pia==1 & VD4002==1 & (V2009 >= 60), VD4020rte, NA),

      
      #Totais
      one1 = ifelse(pia==1 & VD4002==1, VD4016rph, NA),
      one2 = ifelse(pia==1 & VD4002==1, VD4017rpe, NA),
      one3 = ifelse(pia==1 & VD4002==1, VD4019rth, NA),
      one4 = ifelse(pia==1 & VD4002==1, VD4020rte, NA),
      
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
  
  ##Total Geral - Habitual Principal
  estimativas_t1 <- svyby(~one1, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t1)[3]<-"cv_t1"
  
  estimativas_1 <- svyby(~VD4016a, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_1)[3]<-"cv_1"
  
  estimativas_2 <- svyby(~VD4016b, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_2)[3]<-"cv_2"
  
  estimativas_3 <- svyby(~VD4016c, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_3)[3]<-"cv_3"
  
  estimativas_4 <- svyby(~VD4016d, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_4)[3]<-"cv_4"
  
  estimativas_5 <- svyby(~VD4016e, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_5)[3]<-"cv_5"
  
  
  ##Total Geral - Efetivo Principal
  estimativas_t2 <- svyby(~one2, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t2)[3]<-"cv_t2"
  
  estimativas_6 <- svyby(~VD4017a, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_6)[3]<-"cv_6"
  
  estimativas_7 <- svyby(~VD4017b, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_7)[3]<-"cv_7"
  
  estimativas_8 <- svyby(~VD4017c, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_8)[3]<-"cv_8"
  
  estimativas_9 <- svyby(~VD4017d, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_9)[3]<-"cv_9"
  
  estimativas_10 <- svyby(~VD4017e, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_10)[3]<-"cv_10"
  
  
  ##Total Geral - Habitual Todos
  estimativas_t3 <- svyby(~one3, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t3)[3]<-"cv_t3"
  
  estimativas_11 <- svyby(~VD4019a, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_11)[3]<-"cv_11"
  
  estimativas_12 <- svyby(~VD4019b, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_12)[3]<-"cv_12"
  
  estimativas_13 <- svyby(~VD4019c, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_13)[3]<-"cv_13"
  
  estimativas_14 <- svyby(~VD4019d, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_14)[3]<-"cv_14"
  
  estimativas_15 <- svyby(~VD4019e, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_15)[3]<-"cv_15"
  
  
  ##Total Geral - Efetivo Todos
  estimativas_t4 <- svyby(~one4, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_t4)[3]<-"cv_t4"
  
  estimativas_16 <- svyby(~VD4020a, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_16)[3]<-"cv_16"
  
  estimativas_17 <- svyby(~VD4020b, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_17)[3]<-"cv_17"
  
  estimativas_18 <- svyby(~VD4020c, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_18)[3]<-"cv_18"
  
  estimativas_19 <- svyby(~VD4020d, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_19)[3]<-"cv_19"
  
  estimativas_20 <- svyby(~VD4020e, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svymean)
  colnames(estimativas_20)[3]<-"cv_20"
  
  
  
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_t1<- svymean(~one1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t2<- svymean(~one2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t3<- svymean(~one3, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t4<- svymean(~one4, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  total_ocupada1<- svymean(~VD4016a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada2<- svymean(~VD4016b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada3<- svymean(~VD4016c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada4<- svymean(~VD4016d, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada5<- svymean(~VD4016e, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada6<- svymean(~VD4017a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada7<- svymean(~VD4017b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada8<- svymean(~VD4017c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada9<- svymean(~VD4017d, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada10<- svymean(~VD4017e, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada11<- svymean(~VD4019a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada12<- svymean(~VD4019b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada13<- svymean(~VD4019c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada14<- svymean(~VD4019d, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada15<- svymean(~VD4019e, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada16<- svymean(~VD4020a, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada17<- svymean(~VD4020b, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada18<- svymean(~VD4020c, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada19<- svymean(~VD4020d, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada20<- svymean(~VD4020e, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    
    one1 = coef(total_t1),
    cv_t1 = round(cv(total_t1)*100,1),
    VD4016a = coef(total_ocupada1),
    cv_1 = round(cv(total_ocupada1)*100,1),
    VD4016b = coef(total_ocupada2),
    cv_2 = round(cv(total_ocupada2)*100,1),
    VD4016c = coef(total_ocupada3),
    cv_3 = round(cv(total_ocupada3)*100,1),
    VD4016d = coef(total_ocupada4),
    cv_4 = round(cv(total_ocupada4)*100,1),
    VD4016e = coef(total_ocupada5),
    cv_5 = round(cv(total_ocupada5)*100,1),

    
    
    one2 = coef(total_t2),
    cv_t2 = round(cv(total_t2)*100,1),
    VD4017a = coef(total_ocupada6),
    cv_6 = round(cv(total_ocupada6)*100,1),
    VD4017b = coef(total_ocupada7),
    cv_7 = round(cv(total_ocupada7)*100,1),
    VD4017c = coef(total_ocupada8),
    cv_8 = round(cv(total_ocupada8)*100,1),
    VD4017d = coef(total_ocupada9),
    cv_9 = round(cv(total_ocupada9)*100,1),
    VD4017e = coef(total_ocupada10),
    cv_10 = round(cv(total_ocupada10)*100,1),

    
    one3 = coef(total_t3),
    cv_t3 = round(cv(total_t3)*100,1),
    VD4019a = coef(total_ocupada11),
    cv_11 = round(cv(total_ocupada11)*100,1),
    VD4019b = coef(total_ocupada12),
    cv_12 = round(cv(total_ocupada12)*100,1),
    VD4019c = coef(total_ocupada13),
    cv_13 = round(cv(total_ocupada13)*100,1),
    VD4019d = coef(total_ocupada14),
    cv_14 = round(cv(total_ocupada14)*100,1),
    VD4019e = coef(total_ocupada15),
    cv_15 = round(cv(total_ocupada15)*100,1),

    
    one4 = coef(total_t4),
    cv_t4 = round(cv(total_t4)*100,1),
    VD4020a = coef(total_ocupada16),
    cv_16 = round(cv(total_ocupada16)*100,1),
    VD4020b = coef(total_ocupada17),
    cv_17 = round(cv(total_ocupada17)*100,1),
    VD4020c = coef(total_ocupada18),
    cv_18 = round(cv(total_ocupada18)*100,1),
    VD4020d = coef(total_ocupada19),
    cv_19 = round(cv(total_ocupada19)*100,1),
    VD4020e = coef(total_ocupada20),
    cv_20 = round(cv(total_ocupada20)*100,1),

    
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
    left_join(estimativas_t3) %>%
    left_join(estimativas_11) %>% 
    left_join(estimativas_12) %>% 
    left_join(estimativas_13) %>% 
    left_join(estimativas_14) %>% 
    left_join(estimativas_15) %>%
    left_join(estimativas_t4) %>%
    left_join(estimativas_16) %>% 
    left_join(estimativas_17) %>% 
    left_join(estimativas_18) %>% 
    left_join(estimativas_19) %>% 
    left_join(estimativas_20) %>%
    
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_t1, estimativas_t2, estimativas_t3, estimativas_t4, estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, estimativas_12, estimativas_13, estimativas_14, estimativas_15, estimativas_16, estimativas_17, estimativas_18, estimativas_19, estimativas_20, pnadc)
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
                                         "Período" = as.character(df[11, 50]),
                                         
                                         "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[11,2]),
                                         "cv" = as.numeric(df[11,3]),
                                         "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[11,4]),
                                         "cv" = as.numeric(df[11,5]),
                                         "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[11,6]),
                                         "cv" = as.numeric(df[11,7]),
                                         "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[11,8]),
                                         "cv" = as.numeric(df[11,9]),
                                         "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[11,10]),
                                         "cv" = as.numeric(df[11,11]),
                                         "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[11,12]),
                                         "cv" = as.numeric(df[11,13]),
                                         
                                
                                         
                                         "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[11,14]),
                                         "cv" = as.numeric(df[11,15]),
                                         "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[11,16]),
                                         "cv" = as.numeric(df[11,17]),
                                         "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[11,18]),
                                         "cv" = as.numeric(df[11,19]),
                                         "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[11,20]),
                                         "cv" = as.numeric(df[11,21]),
                                         "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[11,22]),
                                         "cv" = as.numeric(df[11,23]),
                                         "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[11,24]),
                                         "cv" = as.numeric(df[11,25]),
                                         
                                         
                                         
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[11,26]),
                                         "cv" = as.numeric(df[11,27]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[11,28]),
                                         "cv" = as.numeric(df[11,29]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[11,30]),
                                         "cv" = as.numeric(df[11,31]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[11,32]),
                                         "cv" = as.numeric(df[11,33]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[11,34]),
                                         "cv" = as.numeric(df[11,35]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[11,36]),
                                         "cv" = as.numeric(df[11,37]),
                                      
                                         
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[11,38]),
                                         "cv" = as.numeric(df[11,39]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[11,40]),
                                         "cv" = as.numeric(df[11,41]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[11,42]),
                                         "cv" = as.numeric(df[11,43]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[11,44]),
                                         "cv" = as.numeric(df[11,45]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[11,46]),
                                         "cv" = as.numeric(df[11,47]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[11,48]),
                                         "cv" = as.numeric(df[11,49])
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
                                         "Período" = as.character(df[1, 50]),
                                         
                                         "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[1,4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[1,6]),
                                         "cv" = as.numeric(df[1,7]),
                                         "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[1,8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[1,10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[1,12]),
                                         "cv" = as.numeric(df[1,13]),
                                         
                                         
                                         
                                         "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[1,14]),
                                         "cv" = as.numeric(df[1,15]),
                                         "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[1,16]),
                                         "cv" = as.numeric(df[1,17]),
                                         "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[1,18]),
                                         "cv" = as.numeric(df[1,19]),
                                         "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[1,20]),
                                         "cv" = as.numeric(df[1,21]),
                                         "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[1,22]),
                                         "cv" = as.numeric(df[1,23]),
                                         "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[1,24]),
                                         "cv" = as.numeric(df[1,25]),
                                         
                                         
                                         
                                         "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[1,26]),
                                         "cv" = as.numeric(df[1,27]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[1,28]),
                                         "cv" = as.numeric(df[1,29]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[1,30]),
                                         "cv" = as.numeric(df[1,31]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[1,32]),
                                         "cv" = as.numeric(df[1,33]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[1,34]),
                                         "cv" = as.numeric(df[1,35]),
                                         "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[1,36]),
                                         "cv" = as.numeric(df[1,37]),
                                         
                                         
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[1,38]),
                                         "cv" = as.numeric(df[1,39]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[1,40]),
                                         "cv" = as.numeric(df[1,41]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[1,42]),
                                         "cv" = as.numeric(df[1,43]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[1,44]),
                                         "cv" = as.numeric(df[1,45]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[1,46]),
                                         "cv" = as.numeric(df[1,47]),
                                         "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[1,48]),
                                         "cv" = as.numeric(df[1,49])
                                         
          
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
                                            13,26,39,52)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2, 50]),
                                                
                                                "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[2,2]),
                                                "cv" = as.numeric(df[2,3]),
                                                "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[2,4]),
                                                "cv" = as.numeric(df[2,5]),
                                                "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[2,6]),
                                                "cv" = as.numeric(df[2,7]),
                                                "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[2,8]),
                                                "cv" = as.numeric(df[2,9]),
                                                "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[2,10]),
                                                "cv" = as.numeric(df[2,11]),
                                                "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[2,12]),
                                                "cv" = as.numeric(df[2,13]),
                                                
                                                
                                                
                                                "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[2,14]),
                                                "cv" = as.numeric(df[2,15]),
                                                "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[2,16]),
                                                "cv" = as.numeric(df[2,17]),
                                                "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[2,18]),
                                                "cv" = as.numeric(df[2,19]),
                                                "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[2,20]),
                                                "cv" = as.numeric(df[2,21]),
                                                "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[2,22]),
                                                "cv" = as.numeric(df[2,23]),
                                                "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[2,24]),
                                                "cv" = as.numeric(df[2,25]),
                                                
                                                
                                                
                                                "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[2,26]),
                                                "cv" = as.numeric(df[2,27]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[2,28]),
                                                "cv" = as.numeric(df[2,29]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[2,30]),
                                                "cv" = as.numeric(df[2,31]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[2,32]),
                                                "cv" = as.numeric(df[2,33]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[2,34]),
                                                "cv" = as.numeric(df[2,35]),
                                                "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[2,36]),
                                                "cv" = as.numeric(df[2,37]),
                                                
                                                
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[2,38]),
                                                "cv" = as.numeric(df[2,39]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[2,40]),
                                                "cv" = as.numeric(df[2,41]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[2,42]),
                                                "cv" = as.numeric(df[2,43]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[2,44]),
                                                "cv" = as.numeric(df[2,45]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[2,46]),
                                                "cv" = as.numeric(df[2,47]),
                                                "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[2,48]),
                                                "cv" = as.numeric(df[2,49])
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
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 50]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[3,2]),
                                              "cv" = as.numeric(df[3,3]),
                                              "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[3,4]),
                                              "cv" = as.numeric(df[3,5]),
                                              "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[3,6]),
                                              "cv" = as.numeric(df[3,7]),
                                              "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[3,8]),
                                              "cv" = as.numeric(df[3,9]),
                                              "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[3,10]),
                                              "cv" = as.numeric(df[3,11]),
                                              "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[3,12]),
                                              "cv" = as.numeric(df[3,13]),
                                              
                                              
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[3,14]),
                                              "cv" = as.numeric(df[3,15]),
                                              "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[3,16]),
                                              "cv" = as.numeric(df[3,17]),
                                              "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[3,18]),
                                              "cv" = as.numeric(df[3,19]),
                                              "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[3,20]),
                                              "cv" = as.numeric(df[3,21]),
                                              "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[3,22]),
                                              "cv" = as.numeric(df[3,23]),
                                              "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[3,24]),
                                              "cv" = as.numeric(df[3,25]),
                                              
                                              
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[3,26]),
                                              "cv" = as.numeric(df[3,27]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[3,28]),
                                              "cv" = as.numeric(df[3,29]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[3,30]),
                                              "cv" = as.numeric(df[3,31]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[3,32]),
                                              "cv" = as.numeric(df[3,33]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[3,34]),
                                              "cv" = as.numeric(df[3,35]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[3,36]),
                                              "cv" = as.numeric(df[3,37]),
                                              
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[3,38]),
                                              "cv" = as.numeric(df[3,39]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[3,40]),
                                              "cv" = as.numeric(df[3,41]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[3,42]),
                                              "cv" = as.numeric(df[3,43]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[3,44]),
                                              "cv" = as.numeric(df[3,45]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[3,46]),
                                              "cv" = as.numeric(df[3,47]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[3,48]),
                                              "cv" = as.numeric(df[3,49])
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
                                       13,26,39,52)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 50]),
                                           
                                           "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[4,4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[4,6]),
                                           "cv" = as.numeric(df[4,7]),
                                           "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[4,8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[4,10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[4,12]),
                                           "cv" = as.numeric(df[4,13]),
                                           
                                           
                                           
                                           "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[4,14]),
                                           "cv" = as.numeric(df[4,15]),
                                           "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[4,16]),
                                           "cv" = as.numeric(df[4,17]),
                                           "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[4,18]),
                                           "cv" = as.numeric(df[4,19]),
                                           "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[4,20]),
                                           "cv" = as.numeric(df[4,21]),
                                           "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[4,22]),
                                           "cv" = as.numeric(df[4,23]),
                                           "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[4,24]),
                                           "cv" = as.numeric(df[4,25]),
                                           
                                           
                                           
                                           "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[4,26]),
                                           "cv" = as.numeric(df[4,27]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[4,28]),
                                           "cv" = as.numeric(df[4,29]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[4,30]),
                                           "cv" = as.numeric(df[4,31]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[4,32]),
                                           "cv" = as.numeric(df[4,33]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[4,34]),
                                           "cv" = as.numeric(df[4,35]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[4,36]),
                                           "cv" = as.numeric(df[4,37]),
                                           
                                           
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[4,38]),
                                           "cv" = as.numeric(df[4,39]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[4,40]),
                                           "cv" = as.numeric(df[4,41]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[4,42]),
                                           "cv" = as.numeric(df[4,43]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[4,44]),
                                           "cv" = as.numeric(df[4,45]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[4,46]),
                                           "cv" = as.numeric(df[4,47]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[4,48]),
                                           "cv" = as.numeric(df[4,49])
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
                                        13,26,39,52)], function(df) {
                                          data.frame(
                                            "Período" = as.character(df[5, 50]),
                                            
                                            "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[5,2]),
                                            "cv" = as.numeric(df[5,3]),
                                            "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[5,4]),
                                            "cv" = as.numeric(df[5,5]),
                                            "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[5,6]),
                                            "cv" = as.numeric(df[5,7]),
                                            "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[5,8]),
                                            "cv" = as.numeric(df[5,9]),
                                            "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[5,10]),
                                            "cv" = as.numeric(df[5,11]),
                                            "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[5,12]),
                                            "cv" = as.numeric(df[5,13]),
                                            
                                            
                                            
                                            "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[5,14]),
                                            "cv" = as.numeric(df[5,15]),
                                            "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[5,16]),
                                            "cv" = as.numeric(df[5,17]),
                                            "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[5,18]),
                                            "cv" = as.numeric(df[5,19]),
                                            "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[5,20]),
                                            "cv" = as.numeric(df[5,21]),
                                            "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[5,22]),
                                            "cv" = as.numeric(df[5,23]),
                                            "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[5,24]),
                                            "cv" = as.numeric(df[5,25]),
                                            
                                            
                                            
                                            "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[5,26]),
                                            "cv" = as.numeric(df[5,27]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[5,28]),
                                            "cv" = as.numeric(df[5,29]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[5,30]),
                                            "cv" = as.numeric(df[5,31]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[5,32]),
                                            "cv" = as.numeric(df[5,33]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[5,34]),
                                            "cv" = as.numeric(df[5,35]),
                                            "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[5,36]),
                                            "cv" = as.numeric(df[5,37]),
                                            
                                            
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[5,38]),
                                            "cv" = as.numeric(df[5,39]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[5,40]),
                                            "cv" = as.numeric(df[5,41]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[5,42]),
                                            "cv" = as.numeric(df[5,43]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[5,44]),
                                            "cv" = as.numeric(df[5,45]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[5,46]),
                                            "cv" = as.numeric(df[5,47]),
                                            "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[5,48]),
                                            "cv" = as.numeric(df[5,49])
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
                                           "Período" = as.character(df[6, 50]),
                                           
                                           "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[6,2]),
                                           "cv" = as.numeric(df[6,3]),
                                           "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[6,4]),
                                           "cv" = as.numeric(df[6,5]),
                                           "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[6,6]),
                                           "cv" = as.numeric(df[6,7]),
                                           "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[6,8]),
                                           "cv" = as.numeric(df[6,9]),
                                           "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[6,10]),
                                           "cv" = as.numeric(df[6,11]),
                                           "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[6,12]),
                                           "cv" = as.numeric(df[6,13]),
                                           
                                           
                                           
                                           "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[6,14]),
                                           "cv" = as.numeric(df[6,15]),
                                           "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[6,16]),
                                           "cv" = as.numeric(df[6,17]),
                                           "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[6,18]),
                                           "cv" = as.numeric(df[6,19]),
                                           "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[6,20]),
                                           "cv" = as.numeric(df[6,21]),
                                           "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[6,22]),
                                           "cv" = as.numeric(df[6,23]),
                                           "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[6,24]),
                                           "cv" = as.numeric(df[6,25]),
                                           
                                           
                                           
                                           "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[6,26]),
                                           "cv" = as.numeric(df[6,27]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[6,28]),
                                           "cv" = as.numeric(df[6,29]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[6,30]),
                                           "cv" = as.numeric(df[6,31]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[6,32]),
                                           "cv" = as.numeric(df[6,33]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[6,34]),
                                           "cv" = as.numeric(df[6,35]),
                                           "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[6,36]),
                                           "cv" = as.numeric(df[6,37]),
                                           
                                           
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[6,38]),
                                           "cv" = as.numeric(df[6,39]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[6,40]),
                                           "cv" = as.numeric(df[6,41]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[6,42]),
                                           "cv" = as.numeric(df[6,43]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[6,44]),
                                           "cv" = as.numeric(df[6,45]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[6,46]),
                                           "cv" = as.numeric(df[6,47]),
                                           "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[6,48]),
                                           "cv" = as.numeric(df[6,49])
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
                                               "Período" = as.character(df[7, 50]),
                                               
                                               "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[7,2]),
                                               "cv" = as.numeric(df[7,3]),
                                               "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[7,4]),
                                               "cv" = as.numeric(df[7,5]),
                                               "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[7,6]),
                                               "cv" = as.numeric(df[7,7]),
                                               "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[7,8]),
                                               "cv" = as.numeric(df[7,9]),
                                               "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[7,10]),
                                               "cv" = as.numeric(df[7,11]),
                                               "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[7,12]),
                                               "cv" = as.numeric(df[7,13]),
                                               
                                               
                                               
                                               "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[7,14]),
                                               "cv" = as.numeric(df[7,15]),
                                               "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[7,16]),
                                               "cv" = as.numeric(df[7,17]),
                                               "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[7,18]),
                                               "cv" = as.numeric(df[7,19]),
                                               "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[7,20]),
                                               "cv" = as.numeric(df[7,21]),
                                               "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[7,22]),
                                               "cv" = as.numeric(df[7,23]),
                                               "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[7,24]),
                                               "cv" = as.numeric(df[7,25]),
                                               
                                               
                                               
                                               "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[7,26]),
                                               "cv" = as.numeric(df[7,27]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[7,28]),
                                               "cv" = as.numeric(df[7,29]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[7,30]),
                                               "cv" = as.numeric(df[7,31]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[7,32]),
                                               "cv" = as.numeric(df[7,33]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[7,34]),
                                               "cv" = as.numeric(df[7,35]),
                                               "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[7,36]),
                                               "cv" = as.numeric(df[7,37]),
                                               
                                               
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[7,38]),
                                               "cv" = as.numeric(df[7,39]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[7,40]),
                                               "cv" = as.numeric(df[7,41]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[7,42]),
                                               "cv" = as.numeric(df[7,43]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[7,44]),
                                               "cv" = as.numeric(df[7,45]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[7,46]),
                                               "cv" = as.numeric(df[7,47]),
                                               "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[7,48]),
                                               "cv" = as.numeric(df[7,49])
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
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[8, 50]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[8,4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[8,6]),
                                              "cv" = as.numeric(df[8,7]),
                                              "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[8,8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[8,10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[8,12]),
                                              "cv" = as.numeric(df[8,13]),
                                              
                                              
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[8,14]),
                                              "cv" = as.numeric(df[8,15]),
                                              "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[8,16]),
                                              "cv" = as.numeric(df[8,17]),
                                              "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[8,18]),
                                              "cv" = as.numeric(df[8,19]),
                                              "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[8,20]),
                                              "cv" = as.numeric(df[8,21]),
                                              "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[8,22]),
                                              "cv" = as.numeric(df[8,23]),
                                              "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[8,24]),
                                              "cv" = as.numeric(df[8,25]),
                                              
                                              
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[8,26]),
                                              "cv" = as.numeric(df[8,27]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[8,28]),
                                              "cv" = as.numeric(df[8,29]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[8,30]),
                                              "cv" = as.numeric(df[8,31]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[8,32]),
                                              "cv" = as.numeric(df[8,33]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[8,34]),
                                              "cv" = as.numeric(df[8,35]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[8,36]),
                                              "cv" = as.numeric(df[8,37]),
                                              
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[8,38]),
                                              "cv" = as.numeric(df[8,39]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[8,40]),
                                              "cv" = as.numeric(df[8,41]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[8,42]),
                                              "cv" = as.numeric(df[8,43]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[8,44]),
                                              "cv" = as.numeric(df[8,45]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[8,46]),
                                              "cv" = as.numeric(df[8,47]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[8,48]),
                                              "cv" = as.numeric(df[8,49])
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
                                              "Período" = as.character(df[9, 50]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[9,2]),
                                              "cv" = as.numeric(df[9,3]),
                                              "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[9,4]),
                                              "cv" = as.numeric(df[9,5]),
                                              "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[9,6]),
                                              "cv" = as.numeric(df[9,7]),
                                              "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[9,8]),
                                              "cv" = as.numeric(df[9,9]),
                                              "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[9,10]),
                                              "cv" = as.numeric(df[9,11]),
                                              "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[9,12]),
                                              "cv" = as.numeric(df[9,13]),
                                              
                                              
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[9,14]),
                                              "cv" = as.numeric(df[9,15]),
                                              "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[9,16]),
                                              "cv" = as.numeric(df[9,17]),
                                              "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[9,18]),
                                              "cv" = as.numeric(df[9,19]),
                                              "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[9,20]),
                                              "cv" = as.numeric(df[9,21]),
                                              "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[9,22]),
                                              "cv" = as.numeric(df[9,23]),
                                              "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[9,24]),
                                              "cv" = as.numeric(df[9,25]),
                                              
                                              
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[9,26]),
                                              "cv" = as.numeric(df[9,27]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[9,28]),
                                              "cv" = as.numeric(df[9,29]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[9,30]),
                                              "cv" = as.numeric(df[9,31]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[9,32]),
                                              "cv" = as.numeric(df[9,33]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[9,34]),
                                              "cv" = as.numeric(df[9,35]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[9,36]),
                                              "cv" = as.numeric(df[9,37]),
                                              
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[9,38]),
                                              "cv" = as.numeric(df[9,39]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[9,40]),
                                              "cv" = as.numeric(df[9,41]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[9,42]),
                                              "cv" = as.numeric(df[9,43]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[9,44]),
                                              "cv" = as.numeric(df[9,45]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[9,46]),
                                              "cv" = as.numeric(df[9,47]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[9,48]),
                                              "cv" = as.numeric(df[9,49])
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
                                              "Período" = as.character(df[10, 50]),
                                              
                                              "Rend. médio mensal habitual - Trab principal - Total" = as.numeric(df[10,2]),
                                              "cv" = as.numeric(df[10,3]),
                                              "Rend. médio mensal habitual - Trab principal - 14 a 17 anos" = as.numeric(df[10,4]),
                                              "cv" = as.numeric(df[10,5]),
                                              "Rend. médio mensal habitual - Trab principal - 18 a 24 anos" = as.numeric(df[10,6]),
                                              "cv" = as.numeric(df[10,7]),
                                              "Rend. médio mensal habitual - Trab principal - 25 a 39 anos" = as.numeric(df[10,8]),
                                              "cv" = as.numeric(df[10,9]),
                                              "Rend. médio mensal habitual - Trab principal - 40 a 59 anos" = as.numeric(df[10,10]),
                                              "cv" = as.numeric(df[10,11]),
                                              "Rend. médio mensal habitual - Trab principal - 60 anos ou mais" = as.numeric(df[10,12]),
                                              "cv" = as.numeric(df[10,13]),
                                              
                                              
                                              
                                              "Rend. médio mensal efetivo - Trab principal - Total" = as.numeric(df[10,14]),
                                              "cv" = as.numeric(df[10,15]),
                                              "Rend. médio mensal efetivo - Trab principal - 14 a 17 anos" = as.numeric(df[10,16]),
                                              "cv" = as.numeric(df[10,17]),
                                              "Rend. médio mensal efetivo - Trab principal - 18 a 24 anos" = as.numeric(df[10,18]),
                                              "cv" = as.numeric(df[10,19]),
                                              "Rend. médio mensal efetivo - Trab principal - 25 a 39 anos" = as.numeric(df[10,20]),
                                              "cv" = as.numeric(df[10,21]),
                                              "Rend. médio mensal efetivo - Trab principal - 40 a 59 anos" = as.numeric(df[10,22]),
                                              "cv" = as.numeric(df[10,23]),
                                              "Rend. médio mensal efetivo - Trab principal - 60 anos ou mais" = as.numeric(df[10,24]),
                                              "cv" = as.numeric(df[10,25]),
                                              
                                              
                                              
                                              "Rend. médio mensal habitual - Todos  os trabalhos - Total" = as.numeric(df[10,26]),
                                              "cv" = as.numeric(df[10,27]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[10,28]),
                                              "cv" = as.numeric(df[10,29]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[10,30]),
                                              "cv" = as.numeric(df[10,31]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[10,32]),
                                              "cv" = as.numeric(df[10,33]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[10,34]),
                                              "cv" = as.numeric(df[10,35]),
                                              "Rend. médio mensal habitual - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[10,36]),
                                              "cv" = as.numeric(df[10,37]),
                                              
                                              
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - Total" = as.numeric(df[10,38]),
                                              "cv" = as.numeric(df[10,39]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 14 a 17 anos" = as.numeric(df[10,40]),
                                              "cv" = as.numeric(df[10,41]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 18 a 24 anos" = as.numeric(df[10,42]),
                                              "cv" = as.numeric(df[10,43]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 25 a 39 anos" = as.numeric(df[10,44]),
                                              "cv" = as.numeric(df[10,45]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 40 a 59 anos" = as.numeric(df[10,46]),
                                              "cv" = as.numeric(df[10,47]),
                                              "Rend. médio mensal efetivo - Todos  os trabalhos - 60 anos ou mais" = as.numeric(df[10,48]),
                                              "cv" = as.numeric(df[10,49])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualrendm5437<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                      "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                      "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                      "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualrendm5437,file = "C:/Users/italo/Desktop/trimestral/baseestratatualrendm5437.rds")

