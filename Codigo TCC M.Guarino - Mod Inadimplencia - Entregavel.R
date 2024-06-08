###########################################################################################
#  
#    BRADESCO - Curso Mineração de Dados PECEGE - 2023/24
#
#         TCC Marli A. Guarino
#
#         Um modelo de identificação de propensão a inadimplência sobre dados públicos
#
#                                                                   junho/2024
#
############################################################################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("readxl","plotly","tidyverse","gridExtra", "PerformanceAnalytics",
             "fBasics", "ggrepel","fastDummies","kableExtra","ggraph","splines",
             "reshape2","correlation","corrplot", "jtools", "cowplot","Hmisc",
             "scatterplot3d", "knitr","kableExtra", "see", "ggraph","psych",
             "nortest","car","ggside","tidyquant","olsrr","ggstance","Rcpp",
             "metan", 
             "lmtest",
             "caret",
             "pROC")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


##******************************************************************************
##*
##*    Primeiro modelo:  Regressão Logística Binária
##*
##*              Ref: Módulo IV aulas 5 a 12 - Professor Wilson
##*
##******************************************************************************


getwd()   # OneDrive/..../Modelos Supervisionados

setwd("C:\\Users\\Dell\\OneDrive\\BRADESCO - Curso Mineração de Dados\\TCC\\DadosStone")

list.files()

# ---------------------- Lendo a base de dados  -------------------------------------

bcoStone <- read.csv( "Dados_Stone corrigido2.csv", header=TRUE, sep=";", dec=".")

#mudando um dos rótulos, para a exploração de dados mostrá-los numa ordem lógica:
e <- which(bcoStone$salario_anual == "menos que $40K")
bcoStone$salario_anual[e]
bcoStone$salario_anual[e] <- "$0K-$40K"
bcoStone$salario_anual[e]

bcoStone_bkp <- bcoStone

View(bcoStone)

which(is.na(bcoStone$limite_credito))
which(is.na(bcoStone$valor_transacoes_12m))

# Estatísticas Simples
summary(bcoStone_bkp)

head(bcoStone)

# Estatísticas descritivas do banco de dados
bcoStone %>% 
  group_by(default) %>% 
  summarise(média_idade = mean(idade, na.rm = T),
            média_dep = mean(dependentes, na.rm = T),
            média_meses_rel = mean(meses_de_relacionamento, na.rm = T),
            média_qt_prod = mean(qtd_produtos, na.rm = T),
            média_lim_cred = mean(limite_credito, na.rm = T),
            média_vlr_trx= mean(valor_transacoes_12m, na.rm = T),
            média_qt_trx= mean(qtd_transacoes_12m, na.rm = T))

table(bcoStone$default[1:20], bcoStone$valor_transacoes_12m[1:20])

table(bcoStone$default, bcoStone$qtd_transacoes_12m)


#=================================================================================
# Estudo da correlacao entre as variaveis
#=================================================================================
cor(bcoStone$default, bcoStone$escolaridade)   # erro: variável explicativa não é numérica
cor(bcoStone$escolaridade, bcoStone$default)   # erro: variável explicativa não é numérica

cor(bcoStone$default, bcoStone$estado_civil)   # erro: variável explicativa não é numérica
cor(bcoStone$default, bcoStone$salario_anual)   # erro: variável explicativa não é numérica
cor(bcoStone$default, bcoStone$meses_de_relacionamento)

explicativas = names(bcoStone)[3:16]  # excluo 1 (id) e 2 (default = var dependente)

# Teste da Correlação
cor.test(bcoStone$default,bcoStone$meses_de_relacionamento)

# ======================      ESTUDO DAS CORRELAÇÕES                           
#A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem

bcoStone[3:16] %>%
  correlation(method = "pearson", size=1, background='gray', text_color='red') %>%
  plot()

#-----------------------------------------------------------------------------------
# Verificando a presença de NA´s
#-----------------------------------------------------------------------------------

library(dplyr)
bcoStone %>% summarise(across(everything(), ~sum(is.na(.))))
summary(bcoStone)

# ***** Por essa função, aparentemente, não há NA´s mas é falso. 
# Olhando a coluna estado_civil, encontramos
# "casado"     "solteiro"   "na"         "divorciado"

# Transformando as variáveis chr em fct

unique(bcoStone$estado_civil)    # para verificar se "na" está na lista
bcoStone <- bcoStone[!bcoStone$estado_civil == "",]        # 749 linhas
dim(bcoStone)
bcoStone$estado_civil <- factor(bcoStone$estado_civil, levels = c("solteiro", "casado","divorciado"))

unique(bcoStone$sexo)
bcoStone$sexo <- factor(bcoStone$sexo, levels = c("M","F"))

unique(bcoStone$escolaridade)
bcoStone <- bcoStone[!bcoStone$escolaridade == "",]        # 749 linhas
dim(bcoStone)
bcoStone$escolaridade <- factor(bcoStone$escolaridade, levels = c("sem educacao formal", "ensino medio",
                                                                  "graduacao", "mestrado", "doutorado"))

unique(bcoStone$salario_anual)
bcoStone <- bcoStone[!bcoStone$salario_anual == "",]        # 749 linhas
dim(bcoStone)
bcoStone$salario_anual <- factor(bcoStone$salario_anual, levels = c("$0K-$40K", "$40K - $60K",  "$60K - $80K",   
                                                                    "$80K - $120K",    "$120K +"))

unique(bcoStone$tipo_cartao)
bcoStone$tipo_cartao <- factor(bcoStone$tipo_cartao, levels = c("blue", "gold", "silver", "platinum"))


view(bcoStone)



# tabulando as frequencias da variavel salario_anual
levels(glimpse(bcoStone$salario_anual)) 
table(bcoStone$salario_anual) 


# GRAFICO DE CORRELAÇÕES MUITO LEGAL! Precisa de dados numéricos na entrada (não serve Factor) !!!  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e respectivas significâncias

chart.Correlation((bcoStone[2:9]), histogram = TRUE)

# ESTE É PARECIDO, mas funciona com factors <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#A função 'pairs.panels' do pacote 'psych' também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
pairs.panels(bcoStone[2:9],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

#A função 'corr_plot' do pacote 'metan' também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias

bcoStone[2:9] %>%
  corr_plot(default, sexo, qtd_produtos, valor_transacoes_12m,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

################################################################################
#             Regressão com variável explicativa qualitativa (dummy)           #
################################################################################

#Visualizando as observações e as especificações das variáveis
glimpse(bcoStone) 


################################################################################
#      Criando n-1 variáveis dummies para cada uma das variáveis categóricas:
#         estado_civil, sexo, escolaridade e salario_anual
################################################################################

# Gerando as dummies para as variáveis categóricas
# A categoria de referência será a dummy mais frequente

bcoStone_dummies <- dummy_columns(.data = bcoStone,
                           select_columns = c("estado_civil", "sexo", "escolaridade", "salario_anual", "tipo_cartao"),
                           remove_selected_columns = T,
                           remove_most_frequent_dummy = T)
names(bcoStone_dummies)

# ----------------   dando nomes mais uteis às colunas (sem "espaços"):   --------------------

nomes = names(bcoStone_dummies)
nomes[16] = "escolaridade_EM"
nomes[18] = "escolaridade_informal"
nomes[19] = "salario_anual_40K_60K"
nomes[20] = "salario_anual_60K_80K"
nomes[21] = "salario_anual_80K_120K"
nomes[22] = "salario_anual_acima_120K"
names(bcoStone_dummies) = nomes

bcoStone_dummies <- bcoStone_dummies[ , 2:25]    # id não será usada nos modelos

view(bcoStone_dummies)


#---------------------------------------------------------------------------------------------
#
# Estimando o modelo de regressão logística binária com as variáveis dummies
#
#---------------------------------------------------------------------------------------------

modelo_Inadimplencia <- glm(formula = default ~ . - id, 
                      data = bcoStone_dummies, 
                      family = "binomial")

# Obtendo os resultados do modelo
summary(modelo_Inadimplencia)

# Valor da Log-Likelihood do modelo
logLik(modelo_Inadimplencia)        # 'log Lik.' -1995.851 (df=24)

#----------------------------
#
# Estimando o modelo de regressão logística binária com as categóricas (não dummyzadas)
modelo_Inadimplencia_cat <- glm(formula = default ~ . - id, 
                            data = bcoStone, 
                            family = "binomial")
# Obtendo os resultados do modelo
summary(modelo_Inadimplencia_cat)

# Valor da Log-Likelihood do modelo
logLik(modelo_Inadimplencia_cat)        # 'log Lik.' -1995.851 (df=24) EXATAMENTE IGUAL

# >>>>>>>>>>>> O resultado é exatamente igual! Não há necessidade de dummies, na regressão logística?

#----------------------------


# Significância estatística geral do modelo (teste qui²)
lrtest(modelo_Inadimplencia)

# Intervalos de confiança para 95% dos parâmetros estimados no modelo
confint.default(modelo_Inadimplencia, level = 0.95)

## O modelo apresenta variáveis que não são estatisticamente significativas
## Ao nível de signifcância de 5%

# Aplicando o procedimento de stepwise no modelo
stepwise_Inadimplencia <- step(object = modelo_Inadimplencia,
                         k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#--------------- Análise do modelo gerado após STEPWISE ------------------

# Obtendo os resultados do modelo pós procedimento de remoção
summary(stepwise_Inadimplencia)

# Valor da Log-Likelihood do modelo final
logLik(stepwise_Inadimplencia)

# Significância estatística geral do modelo (teste qui²)
lrtest(stepwise_Inadimplencia)

# Comparando as Log-Likelihood dos modelos (com e sem stepwise)
lrtest(stepwise_Inadimplencia, modelo_Inadimplencia)

# O teste mostra que a exclusão da variável não alterou a qualidade do ajuste
# Ao nível de sig. de 5%, o modelo com stepwise é preferível

# Intervalos de confiança para 95% dos parâmetros estimados no modelo
confint.default(stepwise_Inadimplencia, level = 0.95)

## Analisando a qualidade do ajuste do modelo final

# Identificando os valores preditos pelo modelo na base de dados atual
valores_preditos <- predict(object = stepwise_Inadimplencia, 
                            data = bcoStone_dummies,
                            type = "response")

round(valores_preditos, 3)

bcoStone_pred_glm <- bcoStone_dummies
bcoStone_pred_glm$vpred <- valores_preditos

bcoStone_pred_glm %>%                           # começando a avaliar a diferença entre valores preditos e reais   
     group_by(default) %>% 
     summarise(média_vpred = mean(vpred, na.rm = T), 
               min_vpred = min(vpred, na.rm = T),
               max_vpred = max(vpred, na.rm = T))

bcoStone_pred_glm[which(bcoStone_pred_glm$default == 1), c(2, 26)]

bcoStone_pred_glm$vpred_int <- round(valores_preditos, 1) * 10
bcoStone_pred_glm$existe <-  1

bcoStone_pred_glm %>%                       # 
  group_by(vpred_int) %>% 
  summarise(soma_inad = sum(default, na.rm = T),
            #total = sum(existe),
            porcent_inad = mean(default, na.rm = T))

  
# Gerando a curva ROC para o modelo final
ROC <- roc(response = bcoStone_dummies$default, 
           predictor = valores_preditos)


ggplotly(
  ggroc(ROC, color = "blue", linewidth = 0.7) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey",
                 linewidth = 0.2) +
    labs(x = "Especificidade",
         y = "Sensibilidade",
         title = paste("Área abaixo da curva (AUC):",
                       round(ROC$auc, 4))) +
    theme_bw())


confusionMatrix(data = factor(ifelse(valores_preditos > 0.7, 1, 0)),
                reference = as.factor(bcoStone_dummies$default), 
                positive = "1")

confusionMatrix(data = factor(ifelse(valores_preditos > 0.6, 1, 0)),
                reference = as.factor(bcoStone_dummies$default), 
                positive = "1")
confusionMatrix(data = factor(ifelse(valores_preditos > 0.4, 1, 0)),
                reference = as.factor(bcoStone_dummies$default), 
                positive = "1")

confusionMatrix(data = factor(ifelse(valores_preditos > 0.3, 1, 0)),
                reference = as.factor(bcoStone_dummies$default), 
                positive = "1")


# Podemos estabelecer um cutoff para a classificação entre evento / não evento
# Aqui, para fins de exemplo, vamos estabelecer um cutoff de 80%
cutoff <- 0.3

# Classificando os valores preditos com base no cutoff
preditos_class = factor(ifelse(valores_preditos > cutoff, 1, 0))

preditos_class

# Matriz de confusão para o cutoff estabelecido
confusionMatrix(data = preditos_class,
                reference = as.factor(bcoStone_dummies$default), 
                positive = "1")

# Realizando previsões com base no modelo final
predict(object = stepwise_Inadimplencia, 
        data.frame(dist = 17, 
                   sem = 10,
                   per_manhã = 1,
                   per_tarde = 0,
                   perfil_calmo = 0,
                   perfil_agressivo = 1), 
        type = "response")


# O modelo estima a probabilidade de xxxx% de se tornar inadimplente

# Analisando as odds ratio do modelo final
odds_ratio <- data.frame(odds_ratio = round(exp(coef(stepwise_Inadimplencia)[-1]),4))

odds_ratio




##***********************************************************************************************
##*
##*
##*     Avaliando modelos do tipo Suport Vector Machine
##*
##*
##***********************************************************************************************

pacotes <- c("e1071",
             "tidyverse",
             "fastDummies",
             "lmtest",
             "caret",
             "pROC",
             "plotly")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Padronizando as variáveis métricas
bcoStone_SVM <- bcoStone_dummies %>% 
  mutate(idade_pad = as.double(scale(idade)),
         dep_pad = as.double(scale(dependentes)),
         trel_pad = as.double(scale(meses_de_relacionamento)),
         qt_prod_pad = as.double(scale(qtd_produtos)),
         iter12m_pad = as.double(scale(iteracoes_12m)),
         inativ12m_pad = as.double(scale(meses_inativo_12m)),
         lim_cred_pad = as.double(scale(limite_credito)),
         vlr_trx_12m_pad = as.double(scale(valor_transacoes_12m)),
         qt_trx_12m_pad = as.double(scale(qtd_transacoes_12m)))

# Resultado da padronização

# Média = 0
round(mean(bcoStone_SVM$lim_cred_pad),3)
round(mean(bcoStone_SVM$qt_trx_12m_pad),3)

# Desvio Padrão = 1
round(sd(bcoStone_SVM$lim_cred_pad),3)
round(sd(bcoStone_SVM$qt_trx_12m_pad),3)

# >>>>>>>>>>>>> a base bcoStone_dummies já está com variáveis categóricas dummizadas  <<<<<<<<<<<<

# Separação da base de dados em bases de treino e teste
set.seed(100)     # usado para que o resultado seja igual em todas as execuções, caso contrário, será aleatório

bcoStone_SVM$id_lin <- 1:nrow(bcoStone_SVM)

treino <- bcoStone_SVM %>% sample_frac(0.70)
teste  <- anti_join(bcoStone_SVM, treino, by = 'id_lin')

treino$id_lin <- NULL
teste$id_lin <- NULL

# não gosta da variável numérica no grid: gerando fatores:
treino$default <- factor(treino$default, levels = c(0, 1))

##---------------------------    Estimando os modelos linear e polinomial   ----------------------------------

# Treinamento do modelo
set.seed(100)

modelo_svm_linear <-  svm(formula = default ~ . - id, 
                          data = treino,
                          type = "C-classification",  # Categórica
                          kernel = "linear",          # "linear" / "polynomial" / "radial"
                          cost = 10,     # custo atribuído aos erros de classificação
                          scale = F)     # já padronizei, não precisa fazer de novo

summary(modelo_svm_linear)
print(modelo_svm_linear)

## Parâmetros da função:
# type: "C-classification" modelo de classificação
# type: "eps-regression" modelo de regressão
# kernel: "linear" / "polynomial" / "radial"
# scale: refere-se à padronização de variáveis métricas (fizemos previamente)

## Hiperparâmetros:
# O "cost" atribui peso às observações que ficam dentro da margem
# Quanto maior o "cost", maior é a penalização atribuída a elas
# cost elevado -> pode tornar a classificação muito rígida (margem menor) e overfitting
# cost baixo -> pode tornar a classificação muito frouxa (margem maior) e underfitting

# Realizando o grid search

set.seed(100)
grid_linear <- tune.svm(inadimpl ~ . - id,
                        data = treino,
                        type = "C-classification",
                        kernel = "linear",
                        tunecontrol = tune.control(cross=5),
                        cost = c(0.01, 0.1, 1, 10, 100))

set.seed(100)
grid_polinomial <- tune.svm(inadimpl ~ . - id,
                            data = treino,
                            type = "C-classification",
                            kernel = "polynomial",
                            tunecontrol = tune.control(cross=5),
                            cost = c(0.01, 0.1, 1, 10, 100))

grid_linear$best.parameters
grid_linear$performances
plot(grid_linear)

grid_polinomial$best.parameters
grid_polinomial$performances
plot(grid_polinomial)

# Parametrizando novamente o modelo
set.seed(100)

modelo_svm_linear <-  svm(formula = inadimpl ~ .-id -default, 
                          data = treino,
                          type = "C-classification",
                          kernel = "linear", 
                          cost = 0.01,
                          scale = F)

modelo_svm_polinomial <-  svm(formula = default ~ .-id,
                              data = treino[, c(1:34)],
                              type = "C-classification",
                              kernel = "polynomial", 
                              cost = 1,
                              scale = F)
summary(modelo_svm_linear)

# Plotando o resultado
plot(modelo_svm_polinomial, 
     data = treino, 
     formula = inadimpl ~ limite_credito,
     color.palette = topo.colors)

plot(modelo_svm_linear, 
     data = treino, 
     formula = idade ~ limite_credito,
     color.palette = topo.colors)

## No argumento "formula" é preciso indicar as 2 variáveis para plotar

#================== Realizando previsões com base no modelo linear
pred_linear_treino <- predict(modelo_svm_linear, treino)

# Matriz de confusão para o modelo linear
confusionMatrix(data = pred_linear_treino,
                reference = treino$inadimpl, 
                positive = "Sim")

# ----------- Avaliando o ajuste na base de testes
pred_linear_teste <- predict(modelo_svm_linear, teste)

pred_linear_teste2 <- teste$default
pred_linear_teste2[which(pred_linear_teste == "Não")] <- 0
pred_linear_teste2[which(pred_linear_teste == "Sim")] <- 1

pred_linear_teste2 <- factor(pred_linear_teste2, levels = c(0, 1))
y_real <- factor(teste$default, levels = c(0, 1))

confusionMatrix(data = pred_linear_teste2,
                reference = y_real, 
                positive = "1")

#================== Realizando previsões com base no modelo polinomial
pred_polinom_treino <- predict(modelo_svm_polinomial, treino)

# Matriz de confusão para o modelo linear
confusionMatrix(data = pred_linear_treino,
                reference = treino$inadimpl, 
                positive = "Sim")

#---------- Avaliando o ajuste na base de testes
pred_polinom_teste <- predict(modelo_svm_polinomial, teste)

resp_teste2 <- teste$default
resp_teste2[which(resp_teste2 == "Não")] <- 0
resp_teste2[which(resp_teste2 == "Sim")] <- 1

resp_teste2 <- factor(pred_linear_teste2, levels = c(0, 1))

confusionMatrix(data = resp_teste2,
                reference = y_real, 
                positive = "1")



#------------------------ Estimando o modelo não linear com RBF ---------------------------------
#
# ****** este modelo não foi concluído - ocorreu algum erro, não tive tempo de explorar ******
#
#------------------------------------------------------------------------------------------------

# Treinamento do modelo
set.seed(100)

modelo_svm_radial <-  svm(formula = atrasado ~ default + sem_pad + per_manhã + perfil_calmo + perfil_moderado + perfil_agressivo, 
                          data = treino,
                          type = "C-classification",
                          kernel = "radial",
                          gamma = 2,
                          cost = 1,
                          scale = F)

modelo_svm_radial <-  svm(formula = default ~ . - id, 
                          data = treino,
                          type = "C-classification",  # Categórica
                          kernel = "radial",          # "linear" / "polynomial" / "radial"
                          gamma = 2,
                          cost = 1,     # custo atribuído aos erros de classificação
                          scale = F)     # já padronizei, não precisa fazer de novo

print(modelo_svm_radial)

## Hiperparâmetros:
# gamma = parâmetro de ajuste da função RBF gaussiana
# Maiores gamma fazem com que a curva em formato de sino fique mais estreita
# Em maiores gamma o intervalo de influência de cada observação é menor
# gamma elevado -> pode tender a uma separação mais complexa
# gamma baixo -> pode tender a uma separação mais linear

# Realizando o grid search
set.seed(100)

grid_radial <- tune.svm(inadimpl ~ . - id - default,
                        data = treino,
                        type = "C-classification",
                        kernel = "radial",
                        tunecontrol = tune.control(cross=5),
                        cost = c(0.01, 0.1, 1, 10, 100),
                        gamma = c(0.1, 0.5, 1.0, 2.0))


grid_radial$best.parameters
grid_radial$performances
plot(grid_radial)

# Parametrizando novamente o modelo
set.seed(100)

modelo_svm_radial <-  svm(formula = default ~ . - id, 
                          data = treino[, c(1:34)],
                          type = "C-classification",  # Categórica
                          kernel = "radial",          # "linear" / "polynomial" / "radial"
                          gamma = 0.1,
                          cost = 1,     # custo atribuído aos erros de classificação
                          scale = F)     # já padronizei, não precisa fazer de novo

# Plotando o resultado
plot(modelo_svm_radial, 
     data = treino, 
     formula = inadimpl ~ limite_credito,
     color.palette = terrain.colors)

plot(modelo_svm_radial, 
     data = treino, 
     formula = idade ~ limite_credito,
     color.palette = terrain.colors)

# Realizando previsões com base no modelo
pred_radial_treino <- predict(modelo_svm_radial, treino)

# Matriz de confusão para o modelo radial
confusionMatrix(data = pred_radial_treino,
                reference = treino$inadimpl, 
                positive = "1")

# Avaliando o ajuste na base de testes
pred_radial_teste <- predict(modelo_svm_radial, teste)

confusionMatrix(data = pred_radial_teste,
                reference = teste, 
                positive = "1")


##***************************************************************************************************
##*
##*
##*    Primeira modelagem multinível utilizando uma variável categórica (Faixa_Salario_anual)
##*
##*
##**************************************************************************************************

install.packages("transformr")


#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl",
             "gghalves","ggdist","tidyquant","car","nlme","lmtest",
             "fastDummies","msm","lmeInfo","jtools","gganimate","ggridges",
             "viridis","hrbrthemes")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##=================================================================================
#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios
#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme
##=================================================================================

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}


#-------------------------------------------------------------------------------
#                          MODELAGEM MULTINÍVEL HLM2                           
#-------------------------------------------------------------------------------

# ************  não temos uma variável ideal para fazer a análise multinível, 
# ************  vamos usar cluster_KMeans e/ou Faixa de renda


#----------------       DESCRIÇÃO E EXPLORAÇÃO DO DATASET:


#Estudo sobre o desbalanceamento dos dados
bcoStone %>%
  group_by(salario_anual) %>%
  summarise(quantidade = n()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)


#default médio  por cluster_KMeans
bcoStone %>%
  group_by(salario_anual) %>%
  summarise(`Probabilidade(default)` = mean(default, na.rm = T)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)


#Exploração visual do default médio
bcoStone %>%
  group_by(salario_anual) %>%
  mutate(default_medio = mean(default, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(aes(x = cluster_KMeans, y = default), color = "red", alpha = 0.5, size = 4) +
  geom_line(aes(x = cluster_KMeans, y = default_medio,
                group = 1, color = "default Médio"), size = 1.5) +
  scale_colour_manual(values = c("default Médio" = "black")) +
  labs(x = "salario_anual",
       y = "default") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))


#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (default), com histograma
ggplotly(
  ggplot(bcoStone, aes(x = default)) +
    geom_density(aes(x = default), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Boxplot da variável dependente (default)
ggplotly(
  ggplot(bcoStone, aes(x = salario_anual, y = default)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "navy") +
    labs(y = "default") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("Boxplot da variável 'default'") +
    xlab("")
)

#Boxplot da variável dependente (default) por Cluster
ggplotly(
  ggplot(bcoStone, aes(x = salario_anual,y = default)) +
    geom_boxplot(aes(fill = salario_anual, alpha = 0.7)) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "dark orchid") +
    scale_fill_viridis_d() +
    labs(y = "default") +
    theme_classic() +
    ggtitle("Boxplots da variável 'default' por cluster_KMeans")
)

#Boxplot alternativo da variável dependente (default) por cluster_KMeans
#pacote 'gghalves'
bcoStone %>%
  ggplot(aes(cluster_KMeans, default, fill = salario_anual)) +
  geom_half_boxplot(
    outlier.colour = "red"
  ) +
  geom_half_dotplot(
    aes(fill = salario_anual),
    dotsize = 0.75,
    alpha = 0.5,
    stackratio = 0.45,
    color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_tq() +
  labs(title = "Boxplots da variável 'default' para cluster_KMeans")

#Gráfico alternativo com distribuições da variável 'default' por cluster_KMeans
#função 'geom_density_ridges_gradient' do pacote 'ggridges'
ggplot(bcoStone, aes(x = default, y = cluster_KMeans, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "default", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'default' por salario_anual",
    x = "default",
    y = "salario_anual") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 3)
  )



ggplot(bcoStone, aes(x = default, y = tipo_cartao, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "default", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'default' por tipo_cartao",
    x = "default",
    y = "Tipo cartão") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 3)
  )




#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (default) por salario_anual
ggplotly(
  ggplot(bcoStone, aes(x = default)) +
    geom_density(aes(color = salario_anual, fill = salario_anual), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)


#Gráfico de default x qtd_produtos (OLS)
ggplotly(
  bcoStone %>%
    ggplot(aes(x = valor_transacoes_12m, y = default)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F,
                color = "red2", size = 1.7) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Valor de transações 12m",
         y = "default") +
    theme_bw()
)



install.packages("gifski")


p <- ggplot(bcoStone, aes(x=qtd_transacoes_12m, y=default, color=salario_anual)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  transition_states(salario_anual, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Quantidade de transações em 12m",
       y = "default") +
  scale_color_viridis_d() +
  ggtitle("default por salario_anual", subtitle = "salario_anual: {closest_state}") +
  theme_minimal()  + 
  geom_point()+
  transition_states(salario_anual)


animate(p, renderer = gifski_renderer(), fps = 4)





#Gráfico de default x salario_anual (visualização do contexto)

#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO (de dependência dos resíduos) NOS DADOS!

ggplotly(legend=T, bcoStone %>%
           ggplot(aes(x = qtd_produtos, y = default, color = salario_anual)) +
           geom_smooth(method = "lm", formula = y ~ x, se = F) +
           geom_point(show_guide = TRUE) +
           #guides(color = "none") +
           theme(legend.position = 'up') + 
           scale_colour_viridis_d() +
           labs(x = "Quantidade de Produtos",
                y = "default") +
           theme_bw()
)

ggplotly(legend=T, bcoStone %>%
           ggplot(aes(x = valor_transacoes_12m, y = default, color = salario_anual)) +
           geom_smooth(method = "lm", formula = y ~ x, se = F) +
           geom_point(show_guide = TRUE) +
           #guides(color = "none") +
           theme(legend.position = 'up') + 
           scale_colour_viridis_d() +
           labs(x = "Valortransações 12m",
                y = "default") +
           theme_bw()
)

#O gráfico a seguir apresenta uma plotagem sob a perspectiva de um modelo com equação única (ex.: OLS)


base_exemplo <- bcoStone %>%  # Linha 334 no gráfico original
  mutate(qtd_produtos = as.numeric(qtd_produtos)) %>%
  mutate(valor_transacoes_12m = as.numeric(valor_transacoes_12m)) 



#===============================================================================
#
#              ESTIMAÇÃO DO MODELO NULO HLM2 com nível 2 = SALARIO ANUAL       #
#
#===============================================================================

# Padronizando as variáveis
bcoStone_padronizado <- as.data.frame(scale(bcoStone_dummies[ , 2:25]))  # já com variáveis dummies
bcoStone_padronizado$default <- bcoStone$default

# Excluindo as dummies de salário, pois vou usar a variável categórica como nível 2 na modelagem
bcoStone_padronizado_hlm2 <- bcoStone_padronizado[ , c(1:17, 22:24)]
bcoStone_padronizado_hlm2$salario_anual <- bcoStone$salario_anual

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = default ~ 1,
                        random = ~ 1 | salario_anual,
                        data = bcoStone_padronizado_hlm2,
                        method = "REML")             #restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)




# ------------------------------------------------------------------------------
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
# ------------------------------------------------------------------------------
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = default ~ 1, 
                      data = bcoStone_padronizado_hlm2)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1,
             color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)


## ***************** Não passa no teste!! Vou !! gerar clusters para usar no nível 2




##***************************************************************************************************
##*
##*
##*    Clusterização K-Means
##*
##*
##**************************************************************************************************

pacotes <- c("plotly",     #plataforma gráfica
             "tidyverse",  #carregar outros pacotes do R
             "ggrepel",    #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2",   #função 'melt'
             "misc3d",     #gráficos 3D
             "plot3D",     #gráficos 3D
             "cluster",    #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4")       #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}




# Gráfico 3D com scatter
rownames(bcoStone) <- bcoStone$id

scatter3D(x=bcoStone$limite_credito,
          y=bcoStone$qtd_produtos,
          z=bcoStone$valor_transacoes_12m,
          phi = 0, bty = "g", pch = 20, cex = 1,
          xlab = "limite_credito",
          ylab = "qtd_produtos",
          zlab = "valor_trx_12m",
          main = "Dados bancários",
          clab = "Inadimplência")>
  text3D(#x=bcoStone$limite_credito,
    #y=bcoStone$qtd_produtos,
    #z=bcoStone$valor_transacoes_12m,
    labels = rownames(bcoStone),
    add = TRUE, cex = 1)

scatter3D(y=bcoStone$limite_credito,
          z=bcoStone$qtd_produtos,
          x=bcoStone$valor_transacoes_12m,
          phi = 0, bty = "g", pch = 20, cex = 1,
          ylab = "limite_credito",
          zlab = "qtd_produtos",
          xlab = "valor_trx_12m",
          main = "Dados bancários",
          clab = "Inadimplência")>
  text3D(#x=bcoStone$limite_credito,
    #y=bcoStone$qtd_produtos,
    #z=bcoStone$valor_transacoes_12m,
    labels = rownames(bcoStone),
    add = TRUE, cex = 1)

scatter3D(z=bcoStone$limite_credito,
          x=bcoStone$qtd_produtos,
          y=bcoStone$valor_transacoes_12m,
          phi = 0, bty = "g", pch = 20, cex = 1,
          zlab = "limite_credito",
          xlab = "qtd_produtos",
          lyab = "valor_trx_12m",
          main = "Dados bancários",
          clab = "Inadimplência")>
  text3D(#x=bcoStone$limite_credito,
    #y=bcoStone$qtd_produtos,
    #z=bcoStone$valor_transacoes_12m,
    labels = rownames(bcoStone),
    add = TRUE, cex = 1)

# Estatísticas descritivas
summary(bcoStone$limite_credito)
summary(bcoStone$qtd_produtos)
summary(bcoStone$valor_transacoes_12m)



#selecionando as colunas a serem utilizadas:
cols <- names(bcoStone_dummies)
cols <- cols[c(3:25)]
cols

glimpse(bcoStone_dummies[,cols])

# Padronizando as variáveis
bcoStone_padronizado <- as.data.frame(scale(bcoStone_dummies[,cols]))  # já com variáveis dummies

rownames(bcoStone_padronizado) <- bcoStone_dummies$id

## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. Por exemplo:

round(mean(bcoStone_padronizado$dependentes),3)
round(sd(bcoStone_padronizado$dependentes))

round(mean(bcoStone_padronizado$limite_credito),3)
round(sd(bcoStone_padronizado$limite_credito))



#=========================================================================================
#                                                                                       ==
#             Esquema de aglomeração por clusterização não hierárquica K-MEANS          ==
#                                                                                       ==
#=========================================================================================

# bcoStone_padronizado = bcoStone_dummies (com dummies) normalizado

# ------- retirando as variáveis que se mostraram não-significativas na primeira avaliação: estado civil
bcoStone_padronizado_k <-  bcoStone_padronizado[ , c(1:9, 12:23)]
names(bcoStone_padronizado_k)

# ------- Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(bcoStone_padronizado_k, kmeans, method = "wss", k.max = 40)


# ------- Executando a clusterização pelo melhor número de clusters identificado pelo método de Elbow:
cluster_KMeans_1 <- kmeans(bcoStone_padronizado_k,
                          centers = 11)

# ------- Criando variável categórica para indicação do cluster no banco de dados
bcoStone$cluster_KMeans <- factor(cluster_KMeans_1$cluster)
bcoStone_dummies$cluster_KMeans <- factor(cluster_KMeans_1$cluster)
bcoStone_padronizado_k$cluster_KMeans <- factor(cluster_KMeans_1$cluster)


#-----------  Visualizando como ficaram os clusters:

bcoStone %>%                           # começando a avaliar a diferença entre valores preditos e reais   
  group_by(cluster_KMeans) %>% 
  summarise(média_default = mean(default, na.rm = T), 
            #min_default = min(default, na.rm = T),
            #max_default = max(default, na.rm = T),
            total = n(),
            qt_default = sum(as.numeric(default))
  )



#-----------  Visualizando como ficaram os clusters:

bcoStone %>%                           # começando a avaliar a diferença entre valores preditos e reais   
  group_by(cluster_KMeans) %>% 
  summarise(média_default = mean(default, na.rm = T), 
            #min_default = min(default, na.rm = T),
            #max_default = max(default, na.rm = T),
            total = n(),
            qt_default = sum(as.numeric(default))
  )


#----------- Análise de variância de um fator (ANOVA)

# - - - -> ANOVA da variável 'limite_credito'
summary(anova_limite_credito <- aov(formula = limite_credito ~ cluster_KMeans,
                                    data = bcoStone_dummies))
summary(anova_limite_credito <- aov(formula = limite_credito ~ cluster_KMeans,
                                    data = bcoStone_dummies))

# - - - -> ANOVA da variável 'qtd_produtos'
summary(anova_qtd_produtos <- aov(formula = qtd_produtos ~ cluster_KMeans,
                                  data = bcoStone_padronizado))
summary(anova_qtd_produtos <- aov(formula = qtd_produtos ~ cluster_KMeans,
                                  data = bcoStone_dummies))

# - - - -> ANOVA da variável 'valor_transacoes_12m'
summary(anova_organiza <- aov(formula = valor_transacoes_12m ~ cluster_KMeans,
                              data = bcoStone_padronizado))
summary(anova_organiza <- aov(formula = valor_transacoes_12m ~ cluster_KMeans,
                              data = bcoStone_dummies))

names(bcoStone_dummies)

summary(anova_organiza <- aov(formula = idade ~ cluster_KMeans, data = bcoStone_dummies))                   
summary(anova_organiza <- aov(formula = dependentes ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = meses_de_relacionamento ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = iteracoes_12m ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = meses_inativo_12m ~ cluster_KMeans, data = bcoStone_dummies))                  
summary(anova_organiza <- aov(formula = qtd_transacoes_12m ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = estado_civil_divorciado ~ cluster_KMeans, data = bcoStone_dummies)) 
summary(anova_organiza <- aov(formula = estado_civil_solteiro ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = sexo_F ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = escolaridade_doutorado ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = escolaridade_EM ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = escolaridade_graduacao ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = escolaridade_informal ~ cluster_KMeans, data = bcoStone_dummies)) 
summary(anova_organiza <- aov(formula = salario_anual_40K_60K ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = salario_anual_60K_80K ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = salario_anual_80K_120K ~ cluster_KMeans, data = bcoStone_dummies))  
summary(anova_organiza <- aov(formula = salario_anual_acima_120K ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = tipo_cartao_gold ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = tipo_cartao_platinum ~ cluster_KMeans, data = bcoStone_dummies))
summary(anova_organiza <- aov(formula = tipo_cartao_silver ~ cluster_KMeans, data = bcoStone_dummies))

# >>>>>>>>>>>   De 22 variáveis usadas, somente duas não tiveram relevância estatística: estado_civil_solteiro e _divorciado



#----------------------------------------------------------------------------------------------
#         Clusterizando com menos variáveis
#----------------------------------------------------------------------------------------------

# ------- retirando as variáveis que não vamos usar
names(bcoStone_padronizado)
bcoStone_padronizado_k2 <-bcoStone_padronizado[ , c(1:9, 12, 17:23)]  
names(bcoStone_padronizado_k2)

# ------- Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(bcoStone_padronizado_k2, kmeans, method = "wss", k.max = 40)

cluster_KMeans_2 <- kmeans(bcoStone_padronizado_k2,
                           centers = 12)

# ------- Criando variável categórica para indicação do cluster no banco de dados
bcoStone_padronizado_k2$cluster_KMeans <- factor(cluster_KMeans_2$cluster)

#----------------------------------------------------------------------------------------------
#         Clusterizando com AINDA menos variáveis
#----------------------------------------------------------------------------------------------

# ------- retirando as variáveis que não vamos usar
names(bcoStone_padronizado)
bcoStone_padronizado_k3 <-bcoStone_padronizado[ , c(1:9, 12, 17:20)]  
names(bcoStone_padronizado_k3)

# ------- Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(bcoStone_padronizado_k3, kmeans, method = "wss", k.max = 40)

cluster_KMeans_3 <- kmeans(bcoStone_padronizado_k3,
                           centers = 12)

# ------- Criando variável categórica para indicação do cluster no banco de dados
bcoStone_padronizado_k3$cluster_KMeans <- factor(cluster_KMeans_3$cluster)

bcoStone$cluster_KMeans1 = bcoStone_padronizado_k$cluster_KMeans
bcoStone$cluster_KMeans2 = bcoStone_padronizado_k2$cluster_KMeans
bcoStone$cluster_KMeans3 = bcoStone_padronizado_k3$cluster_KMeans








##***************************************************************************************************
##*
##*
##*    Segunda modelagem multinível utilizando o cluster K-Means no nível 2
##*
##*
##**************************************************************************************************

#Estudo sobre o desbalanceamento dos dados
bcoStone %>%
  group_by(cluster_KMeans) %>%
  summarise(quantidade = n()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)


#default médio  por cluster_KMeans
bcoStone %>%
  group_by(cluster_KMeans) %>%
  summarise(`Probabilidade(default)` = mean(default, na.rm = T)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# >>>>>>>>>>>>>>> Olhando para o cluster  <<<<<<<<<<<<<<<<<<<<<<<<<<<<
bcoStone %>%
  group_by(cluster_KMeans) %>%
  summarise(quantidade = n()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)



#Exploração visual do default médio
bcoStone %>%
  group_by(cluster_KMeans) %>%
  mutate(default_medio = mean(default, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(aes(x = cluster_KMeans, y = default), color = "red", alpha = 0.5, size = 4) +
  geom_line(aes(x = cluster_KMeans, y = default_medio,
                group = 1, color = "default Médio"), size = 1.5) +
  scale_colour_manual(values = c("default Médio" = "black")) +
  labs(x = "cluster_KMeans",
       y = "default") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))


#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (default), com histograma
ggplotly(
  ggplot(bcoStone, aes(x = default)) +
    geom_density(aes(x = default), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Boxplot da variável dependente (default)
ggplotly(
  ggplot(bcoStone, aes(x = cluster_KMeans, y = default)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "navy") +
    labs(y = "default") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("Boxplot da variável 'default'") +
    xlab("")
)

#Boxplot da variável dependente (default) por Cluster
ggplotly(
  ggplot(bcoStone, aes(x = cluster_KMeans,y = default)) +
    geom_boxplot(aes(fill = cluster_KMeans, alpha = 0.7)) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "dark orchid") +
    scale_fill_viridis_d() +
    labs(y = "default") +
    theme_classic() +
    ggtitle("Boxplots da variável 'default' por cluster_KMeans")
)

#Boxplot alternativo da variável dependente (default) por cluster_KMeans
#pacote 'gghalves'
bcoStone %>%
  ggplot(aes(cluster_KMeans, default, fill = cluster_KMeans)) +
  geom_half_boxplot(
    outlier.colour = "red"
  ) +
  geom_half_dotplot(
    aes(fill = cluster_KMeans),
    dotsize = 0.75,
    alpha = 0.5,
    stackratio = 0.45,
    color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_tq() +
  labs(title = "Boxplots da variável 'default' para cluster_KMeans")

#Gráfico alternativo com distribuições da variável 'default' por cluster_KMeans
#função 'geom_density_ridges_gradient' do pacote 'ggridges'
ggplot(bcoStone, aes(x = default, y = cluster_KMeans, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "default", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'default' por cluster_KMeans",
    x = "default",
    y = "cluster_KMeans") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 3)
  )



ggplot(bcoStone, aes(x = default, y = tipo_cartao, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "default", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'default' por tipo_cartao",
    x = "default",
    y = "Tipo cartão") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 3)
  )




#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (default) por cluster_KMeans
ggplotly(
  ggplot(bcoStone, aes(x = default)) +
    geom_density(aes(color = cluster_KMeans, fill = cluster_KMeans), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)


#Gráfico de default x qtd_produtos (OLS)
ggplotly(
  bcoStone %>%
    ggplot(aes(x = valor_transacoes_12m, y = default)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F,
                color = "red2", size = 1.7) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Valor de transações 12m",
         y = "default") +
    theme_bw()
)



install.packages("gifski")


p <- ggplot(bcoStone, aes(x=qtd_transacoes_12m, y=default, color=cluster_KMeans)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  transition_states(cluster_KMeans, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Quantidade de transações em 12m",
       y = "default cluster_KMeans") +
  scale_color_viridis_d() +
  ggtitle("default por cluster_KMeans", subtitle = "cluster_KMeans: {closest_state}") +
  theme_minimal()  + 
  geom_point()+
  transition_states(cluster_KMeans)


animate(p, renderer = gifski_renderer(), fps = 4)





#Gráfico de default x cluster_KMeans (visualização do contexto)

#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO (de dependência dos resíduos) NOS DADOS!

ggplotly(legend=T, bcoStone %>%
           ggplot(aes(x = qtd_produtos, y = default, color = cluster_KMeans)) +
           geom_smooth(method = "lm", formula = y ~ x, se = F) +
           geom_point(show_guide = TRUE) +
           #guides(color = "none") +
           theme(legend.position = 'up') + 
           scale_colour_viridis_d() +
           labs(x = "Quantidade de Produtos",
                y = "default") +
           theme_bw()
)

ggplotly(legend=T, bcoStone %>%
           ggplot(aes(x = valor_transacoes_12m, y = default, color = cluster_KMeans)) +
           geom_smooth(method = "lm", formula = y ~ x, se = F) +
           geom_point(show_guide = TRUE) +
           #guides(color = "none") +
           theme(legend.position = 'up') + 
           scale_colour_viridis_d() +
           labs(x = "Valortransações 12m",
                y = "default") +
           theme_bw()
)





#==================================================================================================================
#==================================================================================================================
#
#         ESTIMAÇÃO DO MODELO NULO HLM2 com nível 2 = CLUSTERS completos       
#
#==================================================================================================================
#==================================================================================================================

names(bcoStone_padronizado_k)
bcoStone_padronizado_k$default <- bcoStone$default  # pq excluimos a classe destino na clusterização

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_K1 <- lme(fixed = default ~ 1,
                        random = ~ 1 | cluster_KMeans,
                        data = bcoStone_padronizado_k,
                        method = "REML")             #restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_nulo_K1)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_K1)

 # Na trave! o p-valor da comparação é 0.049






#==================================================================================================================
#==================================================================================================================
#
#   ESTIMAÇÃO DO MODELO NULO HLM2 com nível 2 = CLUSTERS com menos variáveis   
#
#==================================================================================================================
#==================================================================================================================


bcoStone_padronizado_k2$default <- bcoStone$default  # pq excluimos a classe destino na clusterização

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_K2 <- lme(fixed = default ~ 1,
                      random = ~ 1 | cluster_KMeans,
                      data = bcoStone_padronizado_k2,
                      method = "REML")             #restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_nulo_K2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_K2)

#    Melhor. Agora, o p-valor é 0,33


#---------------------------------------------------------------------------------------------
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   
#---------------------------------------------------------------------------------------------

#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = default ~ 1, 
                      data = bcoStone_padronizado_k2)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_K2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1,
             color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_K2)


#-------------------------------------------------------------------------------------
#      PASSOU: ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       
#-------------------------------------------------------------------------------------

names(bcoStone_padronizado_k2)

formula_fixed <- paste(
  "default ~ idade + dependentes + meses_de_relacionamento + qtd_produtos " ,
  "+ iteracoes_12m + meses_inativo_12m + limite_credito + valor_transacoes_12m + qtd_transacoes_12m" ,
  " + sexo_F ",
  "+ salario_anual_40K_60K + salario_anual_60K_80K + salario_anual_80K_120K ",
  "+ salario_anual_acima_120K + tipo_cartao_gold + tipo_cartao_platinum + tipo_cartao_silver"
)

formula_random <- paste(
  "~ idade + dependentes + meses_de_relacionamento + qtd_produtos " ,
  " + iteracoes_12m + meses_inativo_12m + limite_credito + valor_transacoes_12m + qtd_transacoes_12m" ,
  " + sexo_F ",
  " + salario_anual_40K_60K + salario_anual_60K_80K + salario_anual_80K_120K + salario_anual_acima_120K ",
  " + tipo_cartao_gold + tipo_cartao_platinum + tipo_cartao_silver",
  " | cluster_KMeans"
)

formula_fixed <- formula(formula_fixed)
formula_fixed

formula_random <- formula(formula_random)
formula_random

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_k2 <- lme(fixed = formula_fixed,          #  <<<<<-------------- Entra em loop aqui, não termina ------------------
                                      random = formula_random,   
                                      data = bcoStone_padronizado_k2,
                                      method = "REML")



#restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_intercept_inclin_k2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_k2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_intercept_inclin_k2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_k2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1,
             color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())





#==================================================================================================================
#==================================================================================================================
#
#   ESTIMAÇÃO DO MODELO NULO HLM2 com nível 2 = CLUSTERS com ainda menos variáveis   
#
#==================================================================================================================
#==================================================================================================================


bcoStone_padronizado_k3$default <- bcoStone$default  # pq excluimos a classe destino na clusterização

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_K3 <- lme(fixed = default ~ 1,
                      random = ~ 1 | cluster_KMeans,
                      data = bcoStone_padronizado_k3,
                      method = "REML")             #restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_nulo_K3)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_K3)

#    Ficou ainda melhor. Agora, o p-valor é 0,25


#-------------------------------------------------------------------------------------
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
#-------------------------------------------------------------------------------------

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_K3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1,
             color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_K3)

# Loklik modelo nulo: -28909     Loglik mod hsml2: 2798.7 (parece bom, ganho > 3% )

#-------------------------------------------------------------------------------------
#      PASSOU: ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       
#-------------------------------------------------------------------------------------

names(bcoStone_padronizado_k3)

formula_fixed <- paste(
  "default ~ idade + dependentes + meses_de_relacionamento + qtd_produtos " ,
  "+ iteracoes_12m + meses_inativo_12m + limite_credito + valor_transacoes_12m + qtd_transacoes_12m" ,
  " + sexo_F ",
  "+ salario_anual_40K_60K + salario_anual_60K_80K + salario_anual_80K_120K ",
  "+ salario_anual_acima_120K"    
           # " + tipo_cartao_gold + tipo_cartao_platinum + tipo_cartao_silver"
)

formula_random <- paste(
  "~ idade + dependentes + meses_de_relacionamento + qtd_produtos " ,
  " + iteracoes_12m + meses_inativo_12m + limite_credito + valor_transacoes_12m + qtd_transacoes_12m" ,
  " + sexo_F ",
  " + salario_anual_40K_60K + salario_anual_60K_80K + salario_anual_80K_120K + salario_anual_acima_120K ",
                     #  " + tipo_cartao_gold + tipo_cartao_platinum + tipo_cartao_silver",
  " | cluster_KMeans"
)

formula_fixed <- formula(formula_fixed)
formula_fixed

formula_random <- formula(formula_random)
formula_random

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_k3 <- lme(fixed = formula_fixed,          #  <<<<<--------------------- Nesta linha, entra em loop e preciso  abortar -----------
                                      random = formula_random,   
                                      data = bcoStone_padronizado_k3,
                                      method = "REML")



#restricted estimation of maximum likelihood (Gelman)

#Parâmetros do modelo
summary(modelo_intercept_inclin_k3)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_k3)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_k3),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_k3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1,
             color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


