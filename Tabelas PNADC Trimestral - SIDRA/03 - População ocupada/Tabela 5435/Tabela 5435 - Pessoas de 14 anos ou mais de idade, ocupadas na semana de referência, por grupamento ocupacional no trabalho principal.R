################################################################################
##    LEITURA E ORGANIZAÇÃO DA PNADC ANUAL POR ESTRATOS GEOGRÁFICOS EM MG     ##
################################################################################

## Arquivos .RDS são bases da própria PNAD
## Leitura e organização do RDS adaptados dos scripts do Paulo

#Tabela 5435 - Pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, por grupamento ocupacional no trabalho principal
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
                                   "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "VD4010", "VD4011", "VD4007", "VD4009", "VD4001", "V2009", "VD4001", "V2007", "V2010", "VD3004", "V4076"))) %>%
    
    update(
      #Criação das variáveis para população, conforme variável VD4007
      pia = as.numeric(V2009 >= 14) ,
      
      ocupada1 = 1 *(pia == 1 & VD4011=="01"),
      ocupada2 = 1 *(pia == 1 & VD4011=="02"),
      ocupada3 = 1 *(pia == 1 & VD4011=="03"),
      ocupada4 = 1 *(pia == 1 & VD4011=="04"),
      ocupada5 = 1 *(pia == 1 & VD4011=="05"),
      ocupada6 = 1 *(pia == 1 & VD4011=="06"),
      ocupada7 = 1 *(pia == 1 & VD4011=="07"),
      ocupada8 = 1 *(pia == 1 & VD4011=="08"),
      ocupada9 = 1 *(pia == 1 & VD4011=="09"),
      ocupada10 = 1 *(pia == 1 & VD4011=="10"),
      ocupada11 = 1 *(pia == 1 & VD4011=="11"),
      
      
      one = 1 * (ocupada1==1 | ocupada2==1 | ocupada3==1 | ocupada4==1 | ocupada5==1 | ocupada6==1 | ocupada7==1 | ocupada8==1 | ocupada9==1 | ocupada10==1 | ocupada11==1),
      
      d.homem = 1 * (V2007 == "Homem"),
      d.mulher = 1 * (V2007 == "Mulher"),
      
      raca_cor = case_when(
        V2010 == "Preta" | V2010 == "Parda" ~ "Menos de 1 mês",
        V2010 == "De 1 mês a menos de 1 ano"  |
          V2010 == "De 1 ano a menos de 2 anos" | V2010 == "Ignorado" ~ "Outras",
        V2010 == "2 anos ou mais" ~ "2 anos ou mais"
      ),
      
      faixa_idade = case_when((V2009 >= 14 &
                                 V2009 <= 29) ~ "0Menos de 1 mêss",
                              (V2009 >= 30 &
                                 V2009 <= 44) ~ "De 1 mês a menos de 1 ano",
                              (V2009 >= 45 &
                                 V2009 <= 59) ~ "De 1 ano a menos de 2 anos",
                              (V2009 >= 60) ~ "2 anos ou mais"
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
  
  ##Total Geral
  estimativas_t <- svyby(~one, by = ~regioes, subset(pnadc, UF == "31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t)[3]<-"cv_t"
  
  ## Total - Diretores e gerentes
  estimativas_1 <- svyby(~ocupada1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_1)[3]<-"cv_1"
  
  ## Total - Profissionais das ciências e intelectuais
  estimativas_2 <- svyby(~ocupada2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_2)[3]<-"cv_2"
  
  ## Total - Técnicos e profissionais de nível médio
  estimativas_3 <- svyby(~ocupada3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_3)[3]<-"cv_3"
  
  ## Total - Trabalhadores de apoio administrativo
  estimativas_4 <- svyby(~ocupada4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_4)[3]<-"cv_4"
  
  ## Total - Trabalhadores dos serviços, vendedores dos comércios e mercados
  estimativas_5 <- svyby(~ocupada5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_5)[3]<-"cv_5"
  
  ## Total - Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca
  estimativas_6 <- svyby(~ocupada6, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_6)[3]<-"cv_6"
  
  ## Total - Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios
  estimativas_7 <- svyby(~ocupada7, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_7)[3]<-"cv_7"
  
  ## Total - Operadores de instalações e máquinas e montadores
  estimativas_8 <- svyby(~ocupada8, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_8)[3]<-"cv_8"
  
  ## Total - Ocupações elementares
  estimativas_9 <- svyby(~ocupada9, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_9)[3]<-"cv_9"
  
  ## Total - Membros das forças armadas, policiais e bombeiros militares
  estimativas_10 <- svyby(~ocupada10, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_10)[3]<-"cv_10"
  
  ## Total - Ocupações mal definidas
  estimativas_11 <- svyby(~ocupada11, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_11)[3]<-"cv_11"
  
  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_t<- svytotal(~one, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_1<- svytotal(~ocupada1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_2<- svytotal(~ocupada2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_3<- svytotal(~ocupada3, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_4<- svytotal(~ocupada4, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_5<- svytotal(~ocupada5, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_6<- svytotal(~ocupada6, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_7<- svytotal(~ocupada7, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_8<- svytotal(~ocupada8, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_9<- svytotal(~ocupada9, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_10<- svytotal(~ocupada10, subset(pnadc,UF=="31"), na.rm = TRUE)
  total_11<- svytotal(~ocupada11, subset(pnadc,UF=="31"), na.rm = TRUE)
  
  
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    
    one = coef(total_t),
    cv_t = round(cv(total_t)*100,1),
    
    ocupada1 = coef(total_1),
    cv_1 = round(cv(total_1)*100,1),
    
    ocupada2 = coef(total_2),
    cv_2 = round(cv(total_2)*100,1),
    
    ocupada3 = coef(total_3),
    cv_3 = round(cv(total_3)*100,1),
    
    ocupada4 = coef(total_4),
    cv_4 = round(cv(total_4)*100,1),
    
    ocupada5 = coef(total_5),
    cv_5 = round(cv(total_5)*100,1),
    
    ocupada6 = coef(total_6),
    cv_6 = round(cv(total_6)*100,1),
    
    ocupada7 = coef(total_7),
    cv_7 = round(cv(total_7)*100,1),
    
    ocupada8 = coef(total_8),
    cv_8 = round(cv(total_8)*100,1),
    
    ocupada9 = coef(total_9),
    cv_9 = round(cv(total_9)*100,1),
    
    ocupada10 = coef(total_10),
    cv_10 = round(cv(total_10)*100,1),
    
    ocupada11 = coef(total_11),
    cv_11 = round(cv(total_11)*100,1),
    
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas_t %>% 
    left_join(estimativas_1) %>% 
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
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_t, estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, pnadc)
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
                                         "Total" = as.numeric(df[11,2]),
                                         "cv" = as.numeric(df[11,3]),
                                         "Diretores e gerentes" = as.numeric(df[11,4]),
                                         "cv" = as.numeric(df[11,5]),
                                         "Profissionais das ciências e intelectuais" = as.numeric(df[11, 6]),
                                         "cv" = as.numeric(df[11,7]),
                                         "Técnicos e profissionais de nível médio" = as.numeric(df[11, 8]),
                                         "cv" = as.numeric(df[11,9]),
                                         "Trabalhadores de apoio administrativo" = as.numeric(df[11,10]),
                                         "cv" = as.numeric(df[11,11]),
                                         "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[11,12]),
                                         "cv" = as.numeric(df[11,13]),
                                         "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[11,14]),
                                         "cv" = as.numeric(df[11,15]),
                                         "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[11,16]),
                                         "cv" = as.numeric(df[11,17]),
                                         "Operadores de instalações e máquinas e montadores" = as.numeric(df[11,18]),
                                         "cv" = as.numeric(df[11,19]),
                                         "Ocupações elementares" = as.numeric(df[11,20]),
                                         "cv" = as.numeric(df[11,21]),
                                         "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[11,22]),
                                         "cv" = as.numeric(df[11,23]),
                                         "Ocupações mal definidas" = as.numeric(df[11,24]),
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
                                         "Total" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "Diretores e gerentes" = as.numeric(df[1,4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "Profissionais das ciências e intelectuais" = as.numeric(df[1, 6]),
                                         "cv" = as.numeric(df[1,7]),
                                         "Técnicos e profissionais de nível médio" = as.numeric(df[1, 8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Trabalhadores de apoio administrativo" = as.numeric(df[1,10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[1,12]),
                                         "cv" = as.numeric(df[1,13]),
                                         "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[1,14]),
                                         "cv" = as.numeric(df[1,15]),
                                         "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[1,16]),
                                         "cv" = as.numeric(df[1,17]),
                                         "Operadores de instalações e máquinas e montadores" = as.numeric(df[1,18]),
                                         "cv" = as.numeric(df[1,19]),
                                         "Ocupações elementares" = as.numeric(df[1,20]),
                                         "cv" = as.numeric(df[1,21]),
                                         "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[1,22]),
                                         "cv" = as.numeric(df[1,23]),
                                         "Ocupações mal definidas" = as.numeric(df[1,24]),
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
                                                "Total" = as.numeric(df[2,2]),
                                                "cv" = as.numeric(df[2,3]),
                                                "Diretores e gerentes" = as.numeric(df[2,4]),
                                                "cv" = as.numeric(df[2,5]),
                                                "Profissionais das ciências e intelectuais" = as.numeric(df[2, 6]),
                                                "cv" = as.numeric(df[2,7]),
                                                "Técnicos e profissionais de nível médio" = as.numeric(df[2, 8]),
                                                "cv" = as.numeric(df[2,9]),
                                                "Trabalhadores de apoio administrativo" = as.numeric(df[2,10]),
                                                "cv" = as.numeric(df[2,11]),
                                                "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[2,12]),
                                                "cv" = as.numeric(df[2,13]),
                                                "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[2,14]),
                                                "cv" = as.numeric(df[2,15]),
                                                "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[2,16]),
                                                "cv" = as.numeric(df[2,17]),
                                                "Operadores de instalações e máquinas e montadores" = as.numeric(df[2,18]),
                                                "cv" = as.numeric(df[2,19]),
                                                "Ocupações elementares" = as.numeric(df[2,20]),
                                                "cv" = as.numeric(df[2,21]),
                                                "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[2,22]),
                                                "cv" = as.numeric(df[2,23]),
                                                "Ocupações mal definidas" = as.numeric(df[2,24]),
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
                                              "Total" = as.numeric(df[3,2]),
                                              "cv" = as.numeric(df[3,3]),
                                              "Diretores e gerentes" = as.numeric(df[3,4]),
                                              "cv" = as.numeric(df[3,5]),
                                              "Profissionais das ciências e intelectuais" = as.numeric(df[3, 6]),
                                              "cv" = as.numeric(df[3,7]),
                                              "Técnicos e profissionais de nível médio" = as.numeric(df[3, 8]),
                                              "cv" = as.numeric(df[3,9]),
                                              "Trabalhadores de apoio administrativo" = as.numeric(df[3,10]),
                                              "cv" = as.numeric(df[3,11]),
                                              "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[3,12]),
                                              "cv" = as.numeric(df[3,13]),
                                              "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[3,14]),
                                              "cv" = as.numeric(df[3,15]),
                                              "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[3,16]),
                                              "cv" = as.numeric(df[3,17]),
                                              "Operadores de instalações e máquinas e montadores" = as.numeric(df[3,18]),
                                              "cv" = as.numeric(df[3,19]),
                                              "Ocupações elementares" = as.numeric(df[3,20]),
                                              "cv" = as.numeric(df[3,21]),
                                              "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[3,22]),
                                              "cv" = as.numeric(df[3,23]),
                                              "Ocupações mal definidas" = as.numeric(df[3,24]),
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
                                           "Total" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "Diretores e gerentes" = as.numeric(df[4,4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "Profissionais das ciências e intelectuais" = as.numeric(df[4, 6]),
                                           "cv" = as.numeric(df[4,7]),
                                           "Técnicos e profissionais de nível médio" = as.numeric(df[4, 8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Trabalhadores de apoio administrativo" = as.numeric(df[4,10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[4,12]),
                                           "cv" = as.numeric(df[4,13]),
                                           "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[4,14]),
                                           "cv" = as.numeric(df[4,15]),
                                           "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[4,16]),
                                           "cv" = as.numeric(df[4,17]),
                                           "Operadores de instalações e máquinas e montadores" = as.numeric(df[4,18]),
                                           "cv" = as.numeric(df[4,19]),
                                           "Ocupações elementares" = as.numeric(df[4,20]),
                                           "cv" = as.numeric(df[4,21]),
                                           "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[4,22]),
                                           "cv" = as.numeric(df[4,23]),
                                           "Ocupações mal definidas" = as.numeric(df[4,24]),
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
                                            "Total" = as.numeric(df[5,2]),
                                            "cv" = as.numeric(df[5,3]),
                                            "Diretores e gerentes" = as.numeric(df[5,4]),
                                            "cv" = as.numeric(df[5,5]),
                                            "Profissionais das ciências e intelectuais" = as.numeric(df[5, 6]),
                                            "cv" = as.numeric(df[5,7]),
                                            "Técnicos e profissionais de nível médio" = as.numeric(df[5, 8]),
                                            "cv" = as.numeric(df[5,9]),
                                            "Trabalhadores de apoio administrativo" = as.numeric(df[5,10]),
                                            "cv" = as.numeric(df[5,11]),
                                            "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[5,12]),
                                            "cv" = as.numeric(df[5,13]),
                                            "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[5,14]),
                                            "cv" = as.numeric(df[5,15]),
                                            "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[5,16]),
                                            "cv" = as.numeric(df[5,17]),
                                            "Operadores de instalações e máquinas e montadores" = as.numeric(df[5,18]),
                                            "cv" = as.numeric(df[5,19]),
                                            "Ocupações elementares" = as.numeric(df[5,20]),
                                            "cv" = as.numeric(df[5,21]),
                                            "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[5,22]),
                                            "cv" = as.numeric(df[5,23]),
                                            "Ocupações mal definidas" = as.numeric(df[5,24]),
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
                                           "Total" = as.numeric(df[6,2]),
                                           "cv" = as.numeric(df[6,3]),
                                           "Diretores e gerentes" = as.numeric(df[6,4]),
                                           "cv" = as.numeric(df[6,5]),
                                           "Profissionais das ciências e intelectuais" = as.numeric(df[6, 6]),
                                           "cv" = as.numeric(df[6,7]),
                                           "Técnicos e profissionais de nível médio" = as.numeric(df[6, 8]),
                                           "cv" = as.numeric(df[6,9]),
                                           "Trabalhadores de apoio administrativo" = as.numeric(df[6,10]),
                                           "cv" = as.numeric(df[6,11]),
                                           "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[6,12]),
                                           "cv" = as.numeric(df[6,13]),
                                           "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[6,14]),
                                           "cv" = as.numeric(df[6,15]),
                                           "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[6,16]),
                                           "cv" = as.numeric(df[6,17]),
                                           "Operadores de instalações e máquinas e montadores" = as.numeric(df[6,18]),
                                           "cv" = as.numeric(df[6,19]),
                                           "Ocupações elementares" = as.numeric(df[6,20]),
                                           "cv" = as.numeric(df[6,21]),
                                           "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[6,22]),
                                           "cv" = as.numeric(df[6,23]),
                                           "Ocupações mal definidas" = as.numeric(df[6,24]),
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
                                         "Total" = as.numeric(df[7,2]),
                                         "cv" = as.numeric(df[7,3]),
                                         "Diretores e gerentes" = as.numeric(df[7,4]),
                                         "cv" = as.numeric(df[7,5]),
                                         "Profissionais das ciências e intelectuais" = as.numeric(df[7, 6]),
                                         "cv" = as.numeric(df[7,7]),
                                         "Técnicos e profissionais de nível médio" = as.numeric(df[7, 8]),
                                         "cv" = as.numeric(df[7,9]),
                                         "Trabalhadores de apoio administrativo" = as.numeric(df[7,10]),
                                         "cv" = as.numeric(df[7,11]),
                                         "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[7,12]),
                                         "cv" = as.numeric(df[7,13]),
                                         "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[7,14]),
                                         "cv" = as.numeric(df[7,15]),
                                         "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[7,16]),
                                         "cv" = as.numeric(df[7,17]),
                                         "Operadores de instalações e máquinas e montadores" = as.numeric(df[7,18]),
                                         "cv" = as.numeric(df[7,19]),
                                         "Ocupações elementares" = as.numeric(df[7,20]),
                                         "cv" = as.numeric(df[7,21]),
                                         "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[7,22]),
                                         "cv" = as.numeric(df[7,23]),
                                         "Ocupações mal definidas" = as.numeric(df[7,24]),
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
                                              "Total" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "Diretores e gerentes" = as.numeric(df[8,4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "Profissionais das ciências e intelectuais" = as.numeric(df[8, 6]),
                                              "cv" = as.numeric(df[8,7]),
                                              "Técnicos e profissionais de nível médio" = as.numeric(df[8, 8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Trabalhadores de apoio administrativo" = as.numeric(df[8,10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[8,12]),
                                              "cv" = as.numeric(df[8,13]),
                                              "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[8,14]),
                                              "cv" = as.numeric(df[8,15]),
                                              "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[8,16]),
                                              "cv" = as.numeric(df[8,17]),
                                              "Operadores de instalações e máquinas e montadores" = as.numeric(df[8,18]),
                                              "cv" = as.numeric(df[8,19]),
                                              "Ocupações elementares" = as.numeric(df[8,20]),
                                              "cv" = as.numeric(df[8,21]),
                                              "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[8,22]),
                                              "cv" = as.numeric(df[8,23]),
                                              "Ocupações mal definidas" = as.numeric(df[8,24]),
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
                                              "Total" = as.numeric(df[9,2]),
                                              "cv" = as.numeric(df[9,3]),
                                              "Diretores e gerentes" = as.numeric(df[9,4]),
                                              "cv" = as.numeric(df[9,5]),
                                              "Profissionais das ciências e intelectuais" = as.numeric(df[9, 6]),
                                              "cv" = as.numeric(df[9,7]),
                                              "Técnicos e profissionais de nível médio" = as.numeric(df[9, 8]),
                                              "cv" = as.numeric(df[9,9]),
                                              "Trabalhadores de apoio administrativo" = as.numeric(df[9,10]),
                                              "cv" = as.numeric(df[9,11]),
                                              "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[9,12]),
                                              "cv" = as.numeric(df[9,13]),
                                              "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[9,14]),
                                              "cv" = as.numeric(df[9,15]),
                                              "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[9,16]),
                                              "cv" = as.numeric(df[9,17]),
                                              "Operadores de instalações e máquinas e montadores" = as.numeric(df[9,18]),
                                              "cv" = as.numeric(df[9,19]),
                                              "Ocupações elementares" = as.numeric(df[9,20]),
                                              "cv" = as.numeric(df[9,21]),
                                              "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[9,22]),
                                              "cv" = as.numeric(df[9,23]),
                                              "Ocupações mal definidas" = as.numeric(df[9,24]),
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
                                              "Total" = as.numeric(df[10,2]),
                                              "cv" = as.numeric(df[10,3]),
                                              "Diretores e gerentes" = as.numeric(df[10,4]),
                                              "cv" = as.numeric(df[10,5]),
                                              "Profissionais das ciências e intelectuais" = as.numeric(df[10, 6]),
                                              "cv" = as.numeric(df[10,7]),
                                              "Técnicos e profissionais de nível médio" = as.numeric(df[10, 8]),
                                              "cv" = as.numeric(df[10,9]),
                                              "Trabalhadores de apoio administrativo" = as.numeric(df[10,10]),
                                              "cv" = as.numeric(df[10,11]),
                                              "Trabalhadores dos serviços, vendedores dos comércios e mercados" = as.numeric(df[10,12]),
                                              "cv" = as.numeric(df[10,13]),
                                              "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca" = as.numeric(df[10,14]),
                                              "cv" = as.numeric(df[10,15]),
                                              "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios" = as.numeric(df[10,16]),
                                              "cv" = as.numeric(df[10,17]),
                                              "Operadores de instalações e máquinas e montadores" = as.numeric(df[10,18]),
                                              "cv" = as.numeric(df[10,19]),
                                              "Ocupações elementares" = as.numeric(df[10,20]),
                                              "cv" = as.numeric(df[10,21]),
                                              "Membros das forças armadas, policiais e bombeiros militares" = as.numeric(df[10,22]),
                                              "cv" = as.numeric(df[10,23]),
                                              "Ocupações mal definidas" = as.numeric(df[10,24]),
                                              "cv" = as.numeric(df[10,25])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatualV5435<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                           "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                           "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                           "10-Central"=central, "11 - Minas Gerais"=mg)

saveRDS(baseestratatualV5435,file = "C:/Users/italo/Desktop/trimestral/baseestratatualV5435.rds")

