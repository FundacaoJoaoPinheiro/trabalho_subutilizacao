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
calcula_ocup_desocup <- function(mesano){
  pnadc <- pnadc_design(read_pnadc(paste0("C:/Users/italo/Desktop/trimestral/txt/PNADC_0",mesano,".txt"), 
                                   "C:/Users/italo/Desktop/trimestral/input/input_PNADC_trimestral.txt",vars=c("VD4002", "V2009", "VD4001", "V2007", "V2010", "VD4009"))) %>%
    update(
           #criação das variáveis da Tabela 4093
           pia = ifelse(V2009 >= 14, 1, 0),
      
           #Pessoas de 14 anos ou mais de idade
           pessoash = 1 * (pia == 1 & V2007 == 1),
           pessoasm = 1 * (pia == 1 & V2007 == 2),
           one1 = ifelse(pia==1, 1, 0),
           
           #Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência
           fth = 1 * (pia == 1 & VD4001 == 1 & V2007==1),
           ftm = 1 * (pia == 1 & VD4001 == 1 & V2007==2),
           one2 = ifelse(pia==1 & VD4001 == 1, 1, 0),
           
           #Pessoas de 14 anos ou mais de idade ocupadas na semana de referência
           ocuph = 1 * (pia == 1 & VD4002==1 & V2007==1),
           ocupm = 1 * (pia == 1 & VD4002==1 & V2007==2),
           one3 = ifelse(pia==1 & VD4002 == 1, 1, 0),
           
           #Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência
           desocuph = 1 * (pia == 1 & VD4002==2 & V2007==1),
           desocupm = 1 * (pia == 1 & VD4002==2 & V2007==2),
           one4 = ifelse(pia==1 & VD4002 == 2, 1, 0),
           
           #Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência
           forafth = 1 * (pia == 1 & VD4001==2 & V2007==1),
           foraftm = 1 * (pia == 1 & VD4001==2 & V2007==2),
           one5 = ifelse(pia==1 & VD4001 == 2, 1, 0),
           
           
           #INFORMAIS
           informais = ifelse(
               (VD4009 == "02" |
               VD4009 == "04" |
               VD4009 == "06" |
               VD4009 == "10"), 1, NA),
           
           informaish = ifelse(pia==1 & informais==1 & V2007==1, 1, NA),
           informaism = ifelse(pia==1 & informais==1 & V2007==2, 1, NA),
           one6 = ifelse(pia==1 & informais==1, 1, NA),
           
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
  
  ## TOTAL
  estimativas_t1 <- svyby(~one1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t1)[3]<-"cv_t1"
  
  ## Total - População com mais de 14 anos, homens
  estimativas_1 <- svyby(~pessoash, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_1)[3]<-"cv1"
  
  ## Total - População com mais de 14 anos, mulheres
  estimativas_2 <- svyby(~pessoasm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_2)[3]<-"cv2"
  
  ## TOTAL
  estimativas_t2 <- svyby(~one2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t2)[3]<-"cv_t2"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens
  estimativas_3 <- svyby(~fth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_3)[3]<-"cv3"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres
  estimativas_4 <- svyby(~ftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_4)[3]<-"cv4"
  
  ## TOTAL
  estimativas_t3 <- svyby(~one3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t3)[3]<-"cv_t3"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens, ocupados
  estimativas_5 <- svyby(~ocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_5)[3]<-"cv5"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres, ocupadas
  estimativas_6 <- svyby(~ocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_6)[3]<-"cv6"
  
  ## TOTAL
  estimativas_t4 <- svyby(~one4, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t4)[3]<-"cv_t4"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, homens, desocupados
  estimativas_7 <- svyby(~desocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_7)[3]<-"cv7"
  
  ## Total - Pessoas com mais de 14 anos na força de trabalho, mulheres, desocupadas
  estimativas_8 <- svyby(~desocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_8)[3]<-"cv8"
  
  ## TOTAL
  estimativas_t5 <- svyby(~one5, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t5)[3]<-"cv_t5"
  
  ## Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência - homens
  estimativas_9 <- svyby(~forafth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_9)[3]<-"cv9"
  
  ## Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência - mulheres
  estimativas_10 <- svyby(~foraftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_10)[3]<-"cv10"
  
  #Total - TPFT
  estimativas_23 <- svyby(~one2, denominator=~one1, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_23)[2]<-"tpft"
  colnames(estimativas_23)[3]<-"cv23"
  
  #Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  estimativas_11 <- svyby(~fth, denominator=~pessoash, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_11)[2]<-"tpfth"
  colnames(estimativas_11)[3]<-"cv11"
  
  #Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  estimativas_12 <- svyby(~ftm, denominator=~pessoasm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_12)[2]<-"tpftm"
  colnames(estimativas_12)[3]<-"cv12"
  
  #Total - NO
  estimativas_24 <- svyby(~one3, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_24)[2]<-"nivelocup"
  colnames(estimativas_24)[3]<-"cv24"
  
  #Nível de ocupação
  #É a razão entre a População Ocupada/ População em Idade de Trabalhar
  estimativas_13 <- svyby(~ocuph, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_13)[2]<-"nivelocuph"
  colnames(estimativas_13)[3]<-"cv13"
  
  #Nível de ocupação
  #É a razão entre a População Ocupada/ População em Idade de Trabalhar
  estimativas_14 <- svyby(~ocupm, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_14)[2]<-"nivelocupm"
  colnames(estimativas_14)[3]<-"cv14"
  
  #Total - ND
  estimativas_25 <- svyby(~one4, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_25)[2]<-"niveldesocup"
  colnames(estimativas_25)[3]<-"cv25"
  
  #Nível de desocupação - homens
  #É a razão entre a População Ocupada/População em Idade de Trabalhar
  estimativas_15 <- svyby(~desocuph, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_15)[2]<-"niveldesocuph"
  colnames(estimativas_15)[3]<-"cv15"
  
  #Nível de desocupação - mulheres
  #É a razão entre a População Ocupada/População em Idade de Trabalhar
  estimativas_16 <- svyby(~desocupm, denominator=~pia, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_16)[2]<-"niveldesocupm"
  colnames(estimativas_16)[3]<-"cv16"
  
  #Total - TXDESOCUP
  estimativas_26 <- svyby(~one4, denominator=~one2, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_26)[2]<-"txdesocup"
  colnames(estimativas_26)[3]<-"cv26"
  
  #Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  estimativas_17 <- svyby(~desocuph, denominator=~fth, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_17)[2]<-"txdesocuph"
  colnames(estimativas_17)[3]<-"cv17"
  
  #Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  estimativas_18 <- svyby(~desocupm, denominator=~ftm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_18)[2]<-"txdesocupm"
  colnames(estimativas_18)[3]<-"cv18"
  
  #Total - TXINF
  estimativas_27 <- svyby(~one6, denominator=~one3, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_27)[2]<-"txinf"
  colnames(estimativas_27)[3]<-"cv27"
  
  #Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%), homens
  estimativas_19 <- svyby(~informaish, denominator=~ocuph, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_19)[2]<-"txinfh"
  colnames(estimativas_19)[3]<-"cv19"
  
  #Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%), mulheres
  estimativas_20 <- svyby(~informaism, denominator=~ocupm, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svyratio)
  colnames(estimativas_20)[2]<-"txinfm"
  colnames(estimativas_20)[3]<-"cv20"
  
  #Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])
  estimativas_21 <- svyby(~informaish, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_21)[3]<-"cv21"
  
  #Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])
  estimativas_22 <- svyby(~informaism, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_22)[3]<-"cv22"
  
  ## TOTAL
  estimativas_t6 <- svyby(~one6, by = ~regioes, subset(pnadc, UF =="31"), vartype="cv", na.rm = TRUE, svytotal)
  colnames(estimativas_t6)[3]<-"cv_t6"
  
  
  
  

  # criando estimativas para o total MG
  
  ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
  ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_t1<- svytotal(~one1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t2<- svytotal(~one2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t3<- svytotal(~one3, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t4<- svytotal(~one4, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t5<- svytotal(~one5, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_t6<- svytotal(~one6, subset(pnadc, UF=="31"),na.rm = TRUE)
  
  total_ocupada1<- svytotal(~pessoash, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada2<- svytotal(~pessoasm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada3<- svytotal(~fth, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada4<- svytotal(~ftm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada5<- svytotal(~ocuph, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada6<- svytotal(~ocupm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada7<- svytotal(~desocuph, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada8<- svytotal(~desocupm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada9<- svytotal(~forafth, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada10<- svytotal(~foraftm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada11<- svyratio(~fth, denominator=~pessoash, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada12<- svyratio(~ftm, denominator=~pessoasm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada13<- svyratio(~ocuph, denominator=~pessoash, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada14<- svyratio(~ocupm, denominator=~pessoasm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada15<- svyratio(~desocuph, denominator=~pessoash, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada16<- svyratio(~desocupm, denominator=~pessoasm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada17<- svyratio(~desocuph, denominator=~fth, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada18<- svyratio(~desocupm, denominator=~ftm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada19<- svyratio(~informaish, denominator=~ocuph, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada20<- svyratio(~informaism, denominator=~ocupm, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada21<- svytotal(~informaish, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada22<- svytotal(~informaism, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada23<- svyratio(~one2, denominator=~one1, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada24<- svyratio(~one3, denominator=~pia, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada25<- svyratio(~one4, denominator=~pia, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada26<- svyratio(~one4, denominator=~one2, subset(pnadc, UF=="31"),na.rm = TRUE)
  total_ocupada27<- svyratio(~one6, denominator=~one3, subset(pnadc, UF=="31"),na.rm = TRUE)
  ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
  ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "09 - Minas Gerais",
    
    one1 = coef(total_t1),
    cv_t1 = round(cv(total_t1)*100,1),
    pessoash = coef(total_ocupada1),
    cv1 = round(cv(total_ocupada1)*100,1),
    pessoasm = coef(total_ocupada2),
    cv2 = round(cv(total_ocupada2)*100,1),
    
    one2 = coef(total_t2),
    cv_t2 = round(cv(total_t2)*100,1),
    fth = coef(total_ocupada3),
    cv3 = round(cv(total_ocupada3)*100,1),
    ftm = coef(total_ocupada4),
    cv4 = round(cv(total_ocupada4)*100,1),
    
    one3 = coef(total_t3),
    cv_t3 = round(cv(total_t3)*100,1),
    ocuph = coef(total_ocupada5),
    cv5 = round(cv(total_ocupada5)*100,1),
    ocupm = coef(total_ocupada6),
    cv6 = round(cv(total_ocupada6)*100,1),
    
    one4 = coef(total_t4),
    cv_t4 = round(cv(total_t4)*100,1),
    desocuph = coef(total_ocupada7),
    cv7 = round(cv(total_ocupada7)*100,1),
    desocupm = coef(total_ocupada8),
    cv8 = round(cv(total_ocupada8)*100,1),
    
    one5 = coef(total_t5),
    cv_t5 = round(cv(total_t5)*100,1),
    forafth = coef(total_ocupada9),
    cv9 = round(cv(total_ocupada9)*100,1),
    foraftm = coef(total_ocupada10),
    cv10 = round(cv(total_ocupada10)*100,1),
    
    
    tpft = coef(total_ocupada23),
    cv23 = round(cv(total_ocupada23)*100,1),
    tpfth = coef(total_ocupada11),
    cv11 = round(cv(total_ocupada11)*100,1),
    tpftm = coef(total_ocupada12),
    cv12 = round(cv(total_ocupada12)*100,1),
    
    nivelocup = coef(total_ocupada24),
    cv24 = round(cv(total_ocupada24)*100,1),
    nivelocuph = coef(total_ocupada13),
    cv13 = round(cv(total_ocupada13)*100,1),
    nivelocupm = coef(total_ocupada14),
    cv14 = round(cv(total_ocupada14)*100,1),
    
    niveldesocup = coef(total_ocupada25),
    cv25 = round(cv(total_ocupada25)*100,1),
    niveldesocuph = coef(total_ocupada15),
    cv15 = round(cv(total_ocupada15)*100,1),
    niveldesocupm = coef(total_ocupada16),
    cv16 = round(cv(total_ocupada16)*100,1),
    
    
    txdesocup = coef(total_ocupada26),
    cv26 = round(cv(total_ocupada26)*100,1),
    txdesocuph = coef(total_ocupada17),
    cv17 = round(cv(total_ocupada17)*100,1),
    txdesocupm = coef(total_ocupada18),
    cv18 = round(cv(total_ocupada18)*100,1),
    
    txinf = coef(total_ocupada27),
    cv27 = round(cv(total_ocupada27)*100,1),
    txinfh = coef(total_ocupada19),
    cv19 = round(cv(total_ocupada19)*100,1),
    txinfm = coef(total_ocupada20),
    cv20 = round(cv(total_ocupada20)*100,1),
    
    one6 = coef(total_t6),
    cv_t6 = round(cv(total_t6)*100,1),
    informaish = coef(total_ocupada21),
    cv21 = round(cv(total_ocupada21)*100,1),
    informaism = coef(total_ocupada22),
    cv22 = round(cv(total_ocupada22)*100,1),
    
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  # Juntando as bases -> estratos
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
    left_join(estimativas_t5) %>%
    left_join(estimativas_9) %>%
    left_join(estimativas_10) %>%
    left_join(estimativas_23) %>%
    left_join(estimativas_11) %>%
    left_join(estimativas_12) %>%
    left_join(estimativas_24) %>%
    left_join(estimativas_13) %>%
    left_join(estimativas_14) %>%
    left_join(estimativas_25) %>%
    left_join(estimativas_15) %>%
    left_join(estimativas_16) %>%
    left_join(estimativas_26) %>%
    left_join(estimativas_17) %>%
    left_join(estimativas_18) %>%
    left_join(estimativas_27) %>%
    left_join(estimativas_19) %>%
    left_join(estimativas_20) %>%
    left_join(estimativas_t6) %>%
    left_join(estimativas_21) %>%
    left_join(estimativas_22) %>%
    
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado

  
  
  rm(estimativas_1, estimativas_2, estimativas_3, estimativas_4, estimativas_5, estimativas_6, estimativas_7, estimativas_8, estimativas_9, estimativas_10, estimativas_11, estimativas_12, estimativas_13, estimativas_14, estimativas_15, estimativas_16, estimativas_17, estimativas_18, estimativas_19, estimativas_20, estimativas_21, estimativas_21, estimativas_22, pnadc)
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
                                         "Período" = as.character(df[9, 68]),
                                         "Total - População com mais de 14 anos" = as.numeric(df[9,2]),
                                         "cv" = as.numeric(df[9,3]),
                                         "População com mais de 14 anos, homens" = as.numeric(df[9, 4]),
                                         "cv" = as.numeric(df[9,5]),
                                         "População com mais de 14 anos, mulheres" = as.numeric(df[9, 6]),
                                         "cv" = as.numeric(df[9,7]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[9,8]),
                                         "cv" = as.numeric(df[9,9]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[9,10]),
                                         "cv" = as.numeric(df[9,11]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[9,12]),
                                         "cv" = as.numeric(df[9,13]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[9,14]),
                                         "cv" = as.numeric(df[9,15]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[9,16]),
                                         "cv" = as.numeric(df[9,17]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[9,18]),
                                         "cv" = as.numeric(df[9,19]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[9,20]),
                                         "cv" = as.numeric(df[9,21]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[9,22]),
                                         "cv" = as.numeric(df[9,23]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[9,24]),
                                         "cv" = as.numeric(df[9,25]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[9,26]),
                                         "cv" = as.numeric(df[9,27]),
                                         "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[9,28]),
                                         "cv" = as.numeric(df[9,29]),
                                         "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[9,30]),
                                         "cv" = as.numeric(df[9,31]),
                                         
                                         "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[9,32]),
                                         "cv" = as.numeric(df[9,33]),
                                         "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[9,34]),
                                         "cv" = as.numeric(df[9,35]),
                                         "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[9,36]),
                                         "cv" = as.numeric(df[9,37]),
                                         
                                         "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[9,38]),
                                         "cv" = as.numeric(df[9,39]),
                                         "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[9,40]),
                                         "cv" = as.numeric(df[9,41]),
                                         "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[9,42]),
                                         "cv" = as.numeric(df[9,43]),
                                         
                                         "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[9,44]),
                                         "cv" = as.numeric(df[9,45]),
                                         "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[9,46]),
                                         "cv" = as.numeric(df[9,47]),
                                         "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[9,48]),
                                         "cv" = as.numeric(df[9,49]),
                                         
                                         "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[9,50]),
                                         "cv" = as.numeric(df[9,51]),
                                         "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[9,52]),
                                         "cv" = as.numeric(df[9,53]),
                                         "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[9,54]),
                                         "cv" = as.numeric(df[9,55]),
                                         
                                         "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[9,56]),
                                         "cv" = as.numeric(df[9,57]),
                                         "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[9,58]),
                                         "cv" = as.numeric(df[9,59]),
                                         "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[9,60]),
                                         "cv" = as.numeric(df[9,61]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[9,62]),
                                         "cv" = as.numeric(df[9,63]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[9,64]),
                                         "cv" = as.numeric(df[9,65]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[9,66]),
                                         "cv" = as.numeric(df[9,67])

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
                                         "Período" = as.character(df[1, 68]),
                                         "Total - População com mais de 14 anos" = as.numeric(df[1,2]),
                                         "cv" = as.numeric(df[1,3]),
                                         "População com mais de 14 anos, homens" = as.numeric(df[1, 4]),
                                         "cv" = as.numeric(df[1,5]),
                                         "População com mais de 14 anos, mulheres" = as.numeric(df[1, 6]),
                                         "cv" = as.numeric(df[1,7]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[1,8]),
                                         "cv" = as.numeric(df[1,9]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[1,10]),
                                         "cv" = as.numeric(df[1,11]),
                                         "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[1,12]),
                                         "cv" = as.numeric(df[1,13]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[1,14]),
                                         "cv" = as.numeric(df[1,15]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[1,16]),
                                         "cv" = as.numeric(df[1,17]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[1,18]),
                                         "cv" = as.numeric(df[1,19]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[1,20]),
                                         "cv" = as.numeric(df[1,21]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[1,22]),
                                         "cv" = as.numeric(df[1,23]),
                                         "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[1,24]),
                                         "cv" = as.numeric(df[1,25]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[1,26]),
                                         "cv" = as.numeric(df[1,27]),
                                         "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[1,28]),
                                         "cv" = as.numeric(df[1,29]),
                                         "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[1,30]),
                                         "cv" = as.numeric(df[1,31]),
                                         
                                         "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[1,32]),
                                         "cv" = as.numeric(df[1,33]),
                                         "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[1,34]),
                                         "cv" = as.numeric(df[1,35]),
                                         "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[1,36]),
                                         "cv" = as.numeric(df[1,37]),
                                         
                                         "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[1,38]),
                                         "cv" = as.numeric(df[1,39]),
                                         "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[1,40]),
                                         "cv" = as.numeric(df[1,41]),
                                         "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[1,42]),
                                         "cv" = as.numeric(df[1,43]),
                                         
                                         "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[1,44]),
                                         "cv" = as.numeric(df[1,45]),
                                         "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[1,46]),
                                         "cv" = as.numeric(df[1,47]),
                                         "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[1,48]),
                                         "cv" = as.numeric(df[1,49]),
                                         
                                         "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[1,50]),
                                         "cv" = as.numeric(df[1,51]),
                                         "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[1,52]),
                                         "cv" = as.numeric(df[1,53]),
                                         "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[1,54]),
                                         "cv" = as.numeric(df[1,55]),
                                         
                                         "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[1,56]),
                                         "cv" = as.numeric(df[1,57]),
                                         "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[1,58]),
                                         "cv" = as.numeric(df[1,59]),
                                         "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[1,60]),
                                         "cv" = as.numeric(df[1,61]),
                                         
                                         "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[1,62]),
                                         "cv" = as.numeric(df[1,63]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[1,64]),
                                         "cv" = as.numeric(df[1,65]),
                                         "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[1,66]),
                                         "cv" = as.numeric(df[1,67])
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
                                                     "Período" = as.character(df[2, 68]),
                                                     "Total - População com mais de 14 anos" = as.numeric(df[2,2]),
                                                     "cv" = as.numeric(df[2,3]),
                                                     "População com mais de 14 anos, homens" = as.numeric(df[2, 4]),
                                                     "cv" = as.numeric(df[2,5]),
                                                     "População com mais de 14 anos, mulheres" = as.numeric(df[2, 6]),
                                                     "cv" = as.numeric(df[2,7]),
                                                     
                                                     "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[2,8]),
                                                     "cv" = as.numeric(df[2,9]),
                                                     "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[2,10]),
                                                     "cv" = as.numeric(df[2,11]),
                                                     "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[2,12]),
                                                     "cv" = as.numeric(df[2,13]),
                                                     
                                                     "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[2,14]),
                                                     "cv" = as.numeric(df[2,15]),
                                                     "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[2,16]),
                                                     "cv" = as.numeric(df[2,17]),
                                                     "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[2,18]),
                                                     "cv" = as.numeric(df[2,19]),
                                                     
                                                     "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[2,20]),
                                                     "cv" = as.numeric(df[2,21]),
                                                     "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[2,22]),
                                                     "cv" = as.numeric(df[2,23]),
                                                     "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[2,24]),
                                                     "cv" = as.numeric(df[2,25]),
                                                     
                                                     "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[2,26]),
                                                     "cv" = as.numeric(df[2,27]),
                                                     "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[2,28]),
                                                     "cv" = as.numeric(df[2,29]),
                                                     "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[2,30]),
                                                     "cv" = as.numeric(df[2,31]),
                                                     
                                                     "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[2,32]),
                                                     "cv" = as.numeric(df[2,33]),
                                                     "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[2,34]),
                                                     "cv" = as.numeric(df[2,35]),
                                                     "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[2,36]),
                                                     "cv" = as.numeric(df[2,37]),
                                                     
                                                     "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[2,38]),
                                                     "cv" = as.numeric(df[2,39]),
                                                     "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[2,40]),
                                                     "cv" = as.numeric(df[2,41]),
                                                     "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[2,42]),
                                                     "cv" = as.numeric(df[2,43]),
                                                     
                                                     "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[2,44]),
                                                     "cv" = as.numeric(df[2,45]),
                                                     "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[2,46]),
                                                     "cv" = as.numeric(df[2,47]),
                                                     "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[2,48]),
                                                     "cv" = as.numeric(df[2,49]),
                                                     
                                                     "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[2,50]),
                                                     "cv" = as.numeric(df[2,51]),
                                                     "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[2,52]),
                                                     "cv" = as.numeric(df[2,53]),
                                                     "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[2,54]),
                                                     "cv" = as.numeric(df[2,55]),
                                                     
                                                     "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[2,56]),
                                                     "cv" = as.numeric(df[2,57]),
                                                     "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[2,58]),
                                                     "cv" = as.numeric(df[2,59]),
                                                     "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[2,60]),
                                                     "cv" = as.numeric(df[2,61]),
                                                     
                                                     "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[2,62]),
                                                     "cv" = as.numeric(df[2,63]),
                                                     "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[2,64]),
                                                     "cv" = as.numeric(df[2,65]),
                                                     "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[2,66]),
                                                     "cv" = as.numeric(df[2,67])
                                                   )
                                                 }))


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
                                            "Período" = as.character(df[3, 68]),
                                            "Total - População com mais de 14 anos" = as.numeric(df[3,2]),
                                            "cv" = as.numeric(df[3,3]),
                                            "População com mais de 14 anos, homens" = as.numeric(df[3, 4]),
                                            "cv" = as.numeric(df[3,5]),
                                            "População com mais de 14 anos, mulheres" = as.numeric(df[3, 6]),
                                            "cv" = as.numeric(df[3,7]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[3,8]),
                                            "cv" = as.numeric(df[3,9]),
                                            "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[3,10]),
                                            "cv" = as.numeric(df[3,11]),
                                            "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[3,12]),
                                            "cv" = as.numeric(df[3,13]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[3,14]),
                                            "cv" = as.numeric(df[3,15]),
                                            "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[3,16]),
                                            "cv" = as.numeric(df[3,17]),
                                            "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[3,18]),
                                            "cv" = as.numeric(df[3,19]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[3,20]),
                                            "cv" = as.numeric(df[3,21]),
                                            "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[3,22]),
                                            "cv" = as.numeric(df[3,23]),
                                            "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[3,24]),
                                            "cv" = as.numeric(df[3,25]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[3,26]),
                                            "cv" = as.numeric(df[3,27]),
                                            "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[3,28]),
                                            "cv" = as.numeric(df[3,29]),
                                            "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[3,30]),
                                            "cv" = as.numeric(df[3,31]),
                                            
                                            "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[3,32]),
                                            "cv" = as.numeric(df[3,33]),
                                            "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[3,34]),
                                            "cv" = as.numeric(df[3,35]),
                                            "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[3,36]),
                                            "cv" = as.numeric(df[3,37]),
                                            
                                            "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[3,38]),
                                            "cv" = as.numeric(df[3,39]),
                                            "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[3,40]),
                                            "cv" = as.numeric(df[3,41]),
                                            "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[3,42]),
                                            "cv" = as.numeric(df[3,43]),
                                            
                                            "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[3,44]),
                                            "cv" = as.numeric(df[3,45]),
                                            "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[3,46]),
                                            "cv" = as.numeric(df[3,47]),
                                            "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[3,48]),
                                            "cv" = as.numeric(df[3,49]),
                                            
                                            "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[3,50]),
                                            "cv" = as.numeric(df[3,51]),
                                            "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[3,52]),
                                            "cv" = as.numeric(df[3,53]),
                                            "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[3,54]),
                                            "cv" = as.numeric(df[3,55]),
                                            
                                            "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[3,56]),
                                            "cv" = as.numeric(df[3,57]),
                                            "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[3,58]),
                                            "cv" = as.numeric(df[3,59]),
                                            "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[3,60]),
                                            "cv" = as.numeric(df[3,61]),
                                            
                                            "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[3,62]),
                                            "cv" = as.numeric(df[3,63]),
                                            "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[3,64]),
                                            "cv" = as.numeric(df[3,65]),
                                            "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[3,66]),
                                            "cv" = as.numeric(df[3,67])
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
                                           "Período" = as.character(df[4, 68]),
                                           "Total - População com mais de 14 anos" = as.numeric(df[4,2]),
                                           "cv" = as.numeric(df[4,3]),
                                           "População com mais de 14 anos, homens" = as.numeric(df[4, 4]),
                                           "cv" = as.numeric(df[4,5]),
                                           "População com mais de 14 anos, mulheres" = as.numeric(df[4, 6]),
                                           "cv" = as.numeric(df[4,7]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[4,8]),
                                           "cv" = as.numeric(df[4,9]),
                                           "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[4,10]),
                                           "cv" = as.numeric(df[4,11]),
                                           "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[4,12]),
                                           "cv" = as.numeric(df[4,13]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[4,14]),
                                           "cv" = as.numeric(df[4,15]),
                                           "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[4,16]),
                                           "cv" = as.numeric(df[4,17]),
                                           "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[4,18]),
                                           "cv" = as.numeric(df[4,19]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[4,20]),
                                           "cv" = as.numeric(df[4,21]),
                                           "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[4,22]),
                                           "cv" = as.numeric(df[4,23]),
                                           "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[4,24]),
                                           "cv" = as.numeric(df[4,25]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[4,26]),
                                           "cv" = as.numeric(df[4,27]),
                                           "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[4,28]),
                                           "cv" = as.numeric(df[4,29]),
                                           "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[4,30]),
                                           "cv" = as.numeric(df[4,31]),
                                           
                                           "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[4,32]),
                                           "cv" = as.numeric(df[4,33]),
                                           "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[4,34]),
                                           "cv" = as.numeric(df[4,35]),
                                           "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[4,36]),
                                           "cv" = as.numeric(df[4,37]),
                                           
                                           "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[4,38]),
                                           "cv" = as.numeric(df[4,39]),
                                           "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[4,40]),
                                           "cv" = as.numeric(df[4,41]),
                                           "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[4,42]),
                                           "cv" = as.numeric(df[4,43]),
                                           
                                           "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[4,44]),
                                           "cv" = as.numeric(df[4,45]),
                                           "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[4,46]),
                                           "cv" = as.numeric(df[4,47]),
                                           "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[4,48]),
                                           "cv" = as.numeric(df[4,49]),
                                           
                                           "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[4,50]),
                                           "cv" = as.numeric(df[4,51]),
                                           "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[4,52]),
                                           "cv" = as.numeric(df[4,53]),
                                           "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[4,54]),
                                           "cv" = as.numeric(df[4,55]),
                                           
                                           "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[4,56]),
                                           "cv" = as.numeric(df[4,57]),
                                           "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[4,58]),
                                           "cv" = as.numeric(df[4,59]),
                                           "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[4,60]),
                                           "cv" = as.numeric(df[4,61]),
                                           
                                           "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[4,62]),
                                           "cv" = as.numeric(df[4,63]),
                                           "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[4,64]),
                                           "cv" = as.numeric(df[4,65]),
                                           "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[4,66]),
                                           "cv" = as.numeric(df[4,67])
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
                                               "Período" = as.character(df[5, 68]),
                                               "Total - População com mais de 14 anos" = as.numeric(df[5,2]),
                                               "cv" = as.numeric(df[5,3]),
                                               "População com mais de 14 anos, homens" = as.numeric(df[5, 4]),
                                               "cv" = as.numeric(df[5,5]),
                                               "População com mais de 14 anos, mulheres" = as.numeric(df[5, 6]),
                                               "cv" = as.numeric(df[5,7]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[5,8]),
                                               "cv" = as.numeric(df[5,9]),
                                               "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[5,10]),
                                               "cv" = as.numeric(df[5,11]),
                                               "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[5,12]),
                                               "cv" = as.numeric(df[5,13]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[5,14]),
                                               "cv" = as.numeric(df[5,15]),
                                               "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[5,16]),
                                               "cv" = as.numeric(df[5,17]),
                                               "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[5,18]),
                                               "cv" = as.numeric(df[5,19]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[5,20]),
                                               "cv" = as.numeric(df[5,21]),
                                               "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[5,22]),
                                               "cv" = as.numeric(df[5,23]),
                                               "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[5,24]),
                                               "cv" = as.numeric(df[5,25]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[5,26]),
                                               "cv" = as.numeric(df[5,27]),
                                               "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[5,28]),
                                               "cv" = as.numeric(df[5,29]),
                                               "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[5,30]),
                                               "cv" = as.numeric(df[5,31]),
                                               
                                               "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[5,32]),
                                               "cv" = as.numeric(df[5,33]),
                                               "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[5,34]),
                                               "cv" = as.numeric(df[5,35]),
                                               "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[5,36]),
                                               "cv" = as.numeric(df[5,37]),
                                               
                                               "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[5,38]),
                                               "cv" = as.numeric(df[5,39]),
                                               "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[5,40]),
                                               "cv" = as.numeric(df[5,41]),
                                               "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[5,42]),
                                               "cv" = as.numeric(df[5,43]),
                                               
                                               "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[5,44]),
                                               "cv" = as.numeric(df[5,45]),
                                               "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[5,46]),
                                               "cv" = as.numeric(df[5,47]),
                                               "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[5,48]),
                                               "cv" = as.numeric(df[5,49]),
                                               
                                               "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[5,50]),
                                               "cv" = as.numeric(df[5,51]),
                                               "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[5,52]),
                                               "cv" = as.numeric(df[5,53]),
                                               "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[5,54]),
                                               "cv" = as.numeric(df[5,55]),
                                               
                                               "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[5,56]),
                                               "cv" = as.numeric(df[5,57]),
                                               "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[5,58]),
                                               "cv" = as.numeric(df[5,59]),
                                               "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[5,60]),
                                               "cv" = as.numeric(df[5,61]),
                                               
                                               "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[5,62]),
                                               "cv" = as.numeric(df[5,63]),
                                               "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[5,64]),
                                               "cv" = as.numeric(df[5,65]),
                                               "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[5,66]),
                                               "cv" = as.numeric(df[5,67])
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
                                                  "Período" = as.character(df[6, 68]),
                                                  "Total - População com mais de 14 anos" = as.numeric(df[6,2]),
                                                  "cv" = as.numeric(df[6,3]),
                                                  "População com mais de 14 anos, homens" = as.numeric(df[6, 4]),
                                                  "cv" = as.numeric(df[6,5]),
                                                  "População com mais de 14 anos, mulheres" = as.numeric(df[6, 6]),
                                                  "cv" = as.numeric(df[6,7]),
                                                  
                                                  "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[6,8]),
                                                  "cv" = as.numeric(df[6,9]),
                                                  "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[6,10]),
                                                  "cv" = as.numeric(df[6,11]),
                                                  "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[6,12]),
                                                  "cv" = as.numeric(df[6,13]),
                                                  
                                                  "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[6,14]),
                                                  "cv" = as.numeric(df[6,15]),
                                                  "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[6,16]),
                                                  "cv" = as.numeric(df[6,17]),
                                                  "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[6,18]),
                                                  "cv" = as.numeric(df[6,19]),
                                                  
                                                  "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[6,20]),
                                                  "cv" = as.numeric(df[6,21]),
                                                  "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[6,22]),
                                                  "cv" = as.numeric(df[6,23]),
                                                  "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[6,24]),
                                                  "cv" = as.numeric(df[6,25]),
                                                  
                                                  "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[6,26]),
                                                  "cv" = as.numeric(df[6,27]),
                                                  "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[6,28]),
                                                  "cv" = as.numeric(df[6,29]),
                                                  "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[6,30]),
                                                  "cv" = as.numeric(df[6,31]),
                                                  
                                                  "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[6,32]),
                                                  "cv" = as.numeric(df[6,33]),
                                                  "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[6,34]),
                                                  "cv" = as.numeric(df[6,35]),
                                                  "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[6,36]),
                                                  "cv" = as.numeric(df[6,37]),
                                                  
                                                  "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[6,38]),
                                                  "cv" = as.numeric(df[6,39]),
                                                  "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[6,40]),
                                                  "cv" = as.numeric(df[6,41]),
                                                  "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[6,42]),
                                                  "cv" = as.numeric(df[6,43]),
                                                  
                                                  "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[6,44]),
                                                  "cv" = as.numeric(df[6,45]),
                                                  "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[6,46]),
                                                  "cv" = as.numeric(df[6,47]),
                                                  "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[6,48]),
                                                  "cv" = as.numeric(df[6,49]),
                                                  
                                                  "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[6,50]),
                                                  "cv" = as.numeric(df[6,51]),
                                                  "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[6,52]),
                                                  "cv" = as.numeric(df[6,53]),
                                                  "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[6,54]),
                                                  "cv" = as.numeric(df[6,55]),
                                                  
                                                  "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[6,56]),
                                                  "cv" = as.numeric(df[6,57]),
                                                  "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[6,58]),
                                                  "cv" = as.numeric(df[6,59]),
                                                  "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[6,60]),
                                                  "cv" = as.numeric(df[6,61]),
                                                  
                                                  "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[6,62]),
                                                  "cv" = as.numeric(df[6,63]),
                                                  "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[6,64]),
                                                  "cv" = as.numeric(df[6,65]),
                                                  "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[6,66]),
                                                  "cv" = as.numeric(df[6,67])
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
                                              "Período" = as.character(df[7, 68]),
                                              "Total - População com mais de 14 anos" = as.numeric(df[7,2]),
                                              "cv" = as.numeric(df[7,3]),
                                              "População com mais de 14 anos, homens" = as.numeric(df[7, 4]),
                                              "cv" = as.numeric(df[7,5]),
                                              "População com mais de 14 anos, mulheres" = as.numeric(df[7, 6]),
                                              "cv" = as.numeric(df[7,7]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[7,8]),
                                              "cv" = as.numeric(df[7,9]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[7,10]),
                                              "cv" = as.numeric(df[7,11]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[7,12]),
                                              "cv" = as.numeric(df[7,13]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[7,14]),
                                              "cv" = as.numeric(df[7,15]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[7,16]),
                                              "cv" = as.numeric(df[7,17]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[7,18]),
                                              "cv" = as.numeric(df[7,19]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[7,20]),
                                              "cv" = as.numeric(df[7,21]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[7,22]),
                                              "cv" = as.numeric(df[7,23]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[7,24]),
                                              "cv" = as.numeric(df[7,25]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[7,26]),
                                              "cv" = as.numeric(df[7,27]),
                                              "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[7,28]),
                                              "cv" = as.numeric(df[7,29]),
                                              "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[7,30]),
                                              "cv" = as.numeric(df[7,31]),
                                              
                                              "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[7,32]),
                                              "cv" = as.numeric(df[7,33]),
                                              "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[7,34]),
                                              "cv" = as.numeric(df[7,35]),
                                              "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[7,36]),
                                              "cv" = as.numeric(df[7,37]),
                                              
                                              "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[7,38]),
                                              "cv" = as.numeric(df[7,39]),
                                              "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[7,40]),
                                              "cv" = as.numeric(df[7,41]),
                                              "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[7,42]),
                                              "cv" = as.numeric(df[7,43]),
                                              
                                              "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[7,44]),
                                              "cv" = as.numeric(df[7,45]),
                                              "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[7,46]),
                                              "cv" = as.numeric(df[7,47]),
                                              "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[7,48]),
                                              "cv" = as.numeric(df[7,49]),
                                              
                                              "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[7,50]),
                                              "cv" = as.numeric(df[7,51]),
                                              "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[7,52]),
                                              "cv" = as.numeric(df[7,53]),
                                              "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[7,54]),
                                              "cv" = as.numeric(df[7,55]),
                                              
                                              "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[7,56]),
                                              "cv" = as.numeric(df[7,57]),
                                              "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[7,58]),
                                              "cv" = as.numeric(df[7,59]),
                                              "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[7,60]),
                                              "cv" = as.numeric(df[7,61]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[7,62]),
                                              "cv" = as.numeric(df[7,63]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[7,64]),
                                              "cv" = as.numeric(df[7,65]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[7,66]),
                                              "cv" = as.numeric(df[7,67])
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
                                              "Período" = as.character(df[8, 68]),
                                              "Total - População com mais de 14 anos" = as.numeric(df[8,2]),
                                              "cv" = as.numeric(df[8,3]),
                                              "População com mais de 14 anos, homens" = as.numeric(df[8, 4]),
                                              "cv" = as.numeric(df[8,5]),
                                              "População com mais de 14 anos, mulheres" = as.numeric(df[8, 6]),
                                              "cv" = as.numeric(df[8,7]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência" = as.numeric(df[8,8]),
                                              "cv" = as.numeric(df[8,9]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - homens" = as.numeric(df[8,10]),
                                              "cv" = as.numeric(df[8,11]),
                                              "Pessoas de 14 anos ou mais de idade, na força de trabalho, na semana de referência - mulheres" = as.numeric(df[8,12]),
                                              "cv" = as.numeric(df[8,13]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade ocupadas na semana de referência" = as.numeric(df[8,14]),
                                              "cv" = as.numeric(df[8,15]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - homens" = as.numeric(df[8,16]),
                                              "cv" = as.numeric(df[8,17]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência - mulheres" = as.numeric(df[8,18]),
                                              "cv" = as.numeric(df[8,19]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)" = as.numeric(df[8,20]),
                                              "cv" = as.numeric(df[8,21]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - homens" = as.numeric(df[8,22]),
                                              "cv" = as.numeric(df[8,23]),
                                              "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[8,24]),
                                              "cv" = as.numeric(df[8,25]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)" = as.numeric(df[8,26]),
                                              "cv" = as.numeric(df[8,27]),
                                              "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - homens" = as.numeric(df[8,28]),
                                              "cv" = as.numeric(df[8,29]),
                                              "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas) - mulheres" = as.numeric(df[8,30]),
                                              "cv" = as.numeric(df[8,31]),
                                              
                                              "Total - Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[8,32]),
                                              "cv" = as.numeric(df[8,33]),
                                              "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[8,34]),
                                              "cv" = as.numeric(df[8,35]),
                                              "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[8,36]),
                                              "cv" = as.numeric(df[8,37]),
                                              
                                              "Total - Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[8,38]),
                                              "cv" = as.numeric(df[8,39]),
                                              "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[8,40]),
                                              "cv" = as.numeric(df[8,41]),
                                              "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[8,42]),
                                              "cv" = as.numeric(df[8,43]),
                                              
                                              "Total - Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[8,44]),
                                              "cv" = as.numeric(df[8,45]),
                                              "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[8,46]),
                                              "cv" = as.numeric(df[8,47]),
                                              "Nível da desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[8,48]),
                                              "cv" = as.numeric(df[8,49]),
                                              
                                              "Total - Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[8,50]),
                                              "cv" = as.numeric(df[8,51]),
                                              "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[8,52]),
                                              "cv" = as.numeric(df[8,53]),
                                              "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[8,54]),
                                              "cv" = as.numeric(df[8,55]),
                                              
                                              "Total - Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%)" = as.numeric(df[8,56]),
                                              "cv" = as.numeric(df[8,57]),
                                              "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - homens" = as.numeric(df[8,58]),
                                              "cv" = as.numeric(df[8,59]),
                                              "Taxa de informalidade, na semana de referência, das pessoas de 14 anos ou mais de idade (%) - mulheres" = as.numeric(df[8,60]),
                                              "cv" = as.numeric(df[8,61]),
                                              
                                              "Total - Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024])" = as.numeric(df[8,62]),
                                              "cv" = as.numeric(df[8,63]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - homens" = as.numeric(df[8,64]),
                                              "cv" = as.numeric(df[8,65]),
                                              "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas [4º trimestre 2015 a 4º trimestre 2024]) - mulheres" = as.numeric(df[8,66]),
                                              "cv" = as.numeric(df[8,67])
                                            )
                                          }))

#View(central)

### SALVANDO ARQUIVO LISTA #####################################################
baseestratatual4093<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano + Colar de BH"=entornobhcolar,
                          "03-Sul de Minas"=sulmg, "04-Triângulo Mineiro"=trng,
                          "05-Mata de Minas Gerais"=zonamata, "06-Norte de Minas + RIDE de Brasilia em MG"=nortemgride, "07-Vale do Rio Doce"=riodoce,
                          "08-Central"=central, "09 - Minas Gerais"=mg)

saveRDS(baseestratatual4093,file = "C:/Users/italo/Desktop/trimestral/baseestratatual4093.rds")


