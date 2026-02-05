# ---BIBLIOTECAS E DADOS ---
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(writexl)) install.packages("writexl")
install.packages("ggcorrplot")
install.packages("caret")
install.packages("PRROC")
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggcorrplot)
library(caret)
library(PRROC)
dados <-default_of_credit_card_clients_ #Exportar o dataset
dados <- dados %>% select(-ID) #Remover o identificadores
str(dados)
summary(dados[, c("LIMIT_BAL", "AGE")])
table(dados$`default payment next month`) %>% prop.table()

# --- 3. ANÁLISE EXPLORATÓRIA ---
# Histograma da Idade (AGE)
ggplot(dados, aes(x = AGE)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribuição de Idade dos Clientes", x = "Idade", y = "Frequência")
# Gráfico de Barras para Educação
ggplot(dados, aes(x = factor(EDUCATION))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribuição por Nível de Escolaridade", x = "Educação")
#Taxa de Incumprimento por Género (1=Masc, 2=Fem)
ggplot(dados, aes(x = factor(SEX), fill = factor(`default payment next month`))) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1", name = "Incumprimento") +
  labs(title = "Proporção de Incumprimento por Género", x = "Sexo (1=M, 2=F)", y = "Proporção")
# Limite de Crédito
ggplot(dados, aes(x = LIMIT_BAL)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "#56B4E9", 
                 color = "white") +
  geom_density(alpha = 0.2, fill = "#0072B2") +
  labs(title = "Distribuição de Frequência: Limite de Crédito",
       x = "Limite de Crédito (LIMIT_BAL)",
       y = "Densidade / Frequência") +
  theme_minimal()
#Default.payment.next.month 
ggplot(dados, aes(x = factor(`default payment next month`), fill = factor(`default payment next month`))) +
  geom_bar() +
  scale_fill_manual(values = c("steelblue", "firebrick"), name = "Incumprimento") +
  labs(title = "Contagem de Incumprimento (Y)",
       x = "0 = Não, 1 = Sim",
       y = "Número de Clientes") +
  theme_minimal()

#BILL-AMT1-6
# Selecionar as colunas de faturas
faturas <- dados[, c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")]
# Resumo estatístico
summary(faturas)
# Transformar os dados para formato "longo" para o ggplot
faturas_long <- melt(faturas)

ggplot(faturas_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribuição dos Valores de Fatura (Abril - Setembro)",
       x = "Mês da Fatura",
       y = "Valor (NT$)") +
  theme_minimal() +
  theme(legend.position = "none")
#Marriage
#Preparar os dados (Tratar o 0 e transformar em labels)
dados$MARRIAGE_LAB <- factor(dados$MARRIAGE, 
                             levels = c(1, 2, 3, 0), 
                             labels = c("Casado", "Solteiro", "Outros", "Outros"))
ggplot(dados, aes(x = MARRIAGE_LAB, fill = factor(`default payment next month`))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#2b8cbe", "#e31a1c"), 
                    name = "Incumprimento", 
                    labels = c("Não (0)", "Sim (1)")) +
  labs(title = "Proporção de Incumprimento por Estado Civil",
       x = "Estado Civil",
       y = "Percentagem (%)") +
  theme_minimal()


# --- 4.Relação entre variáveis --- ---

#Estado do pagamento recente vs Incumprimento
faturas <- dados[, c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")]
dados$default.payment.next.month <- as.factor(dados$`default payment next month`)
dados_plot <- dados %>%
  mutate(status_pag = case_when(
    PAY_0 <= 0 ~ "Em dia",
    PAY_0 == 1 ~ "1 mês atraso",
    PAY_0 == 2 ~ "2 meses atraso",
    PAY_0 >= 3 ~ "3+ meses atraso"
  ))
dados_plot$status_pag <- factor(dados_plot$status_pag, 
                                levels = c("Em dia", "1 mês atraso", "2 meses atraso", "3+ meses atraso"))
ggplot(dados_plot, aes(x = status_pag, fill = factor(default.payment.next.month))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#3182bd", "#de2d26"), 
                    name = "Incumprimento", 
                    labels = c("Não (0)", "Sim (1)")) +
  labs(title = "Probabilidade de Incumprimento vs. Estado de Pagamento Recente",
       x = "Estado de Pagamento em Setembro (PAY_0)",
       y = "Percentagem de Clientes") +
  theme_minimal()

#Limite de Crédito vs Incumprimento
ggplot(dados, aes(x = factor(default.payment.next.month), y = LIMIT_BAL, fill = factor(default.payment.next.month))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) + # Formatar números grandes
  scale_fill_manual(values = c("#2b8cbe", "#e31a1c"), 
                    name = "Incumprimento", 
                    labels = c("Não (0)", "Sim (1)")) +
  labs(title = "Distribuição do Limite de Crédito por Estado de Pagamento",
       x = "Incumprimento no Mês Seguinte",
       y = "Limite de Crédito (NT$)") +
  theme_minimal()

#Correlação entre faturas 

faturas_cols <- dados[, c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")]
corr_faturas <- cor(faturas_cols)
ggcorrplot(corr_faturas, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           title = "Matriz de Correlação: BILL_AMT1 - BILL_AMT6",
           colors = c("#6D9ECB", "white", "#E46726"))

#Escolaridade vs Incumprimento 
dados$EDUCATION_LIMPO <- ifelse(dados$EDUCATION %in% c(0, 4, 5, 6), 4, dados$EDUCATION)
dados$EDUCATION_LIMPO <- factor(dados$EDUCATION_LIMPO, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Pós-Grad", "Univ", "Ens. Médio", "Outros"))

ggplot(dados, aes(x = EDUCATION_LIMPO, fill = factor(default.payment.next.month))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#2b8cbe", "#e31a1c"), name = "Incumprimento") +
  labs(title = "Risco de Incumprimento por Nível de Escolaridade",
       x = "Nível de Educação", y = "Percentagem (%)") +
  theme_minimal()

# --- 5. Qualidade dos Dados ---
#Duplicados
num_duplicados <- sum(duplicated(dados))
cat("Número de registos duplicados:", num_duplicados)
if(num_duplicados > 0) {
  dados <- dados %>% distinct()
}
# Verificar NAs por coluna
colSums(is.na(dados))

#Valores inconsistentes ou inválidos 
# Recodificar Education (0, 4, 5, 6 -> 4 "Outros")
dados$EDUCATION <- ifelse(dados$EDUCATION %in% c(0, 5, 6), 4, dados$EDUCATION)

# Recodificar Marriage (0 -> 3 "Outros")
dados$MARRIAGE <- ifelse(dados$MARRIAGE == 0, 3, dados$MARRIAGE)
dados$default.payment.next.month <- factor(
  dados$`default payment next month`,
  levels = c(0, 1),
  labels = c("NoDefault", "Default")
)

# --- 6. Divisão Train/Test e Replicabilidade ---
# Garantir a Replicabilidade (Escolha um número, ex: 123)
set.seed(123)
indice_treino <- createDataPartition(dados$`default payment next month`, p = 0.75, list = FALSE)
treino <- dados[indice_treino, ]
teste  <- dados[-indice_treino, ]
# Exportar para excel
write.csv(treino, "train.csv", row.names = FALSE)
write.csv(teste,  "test.csv",  row.names = FALSE)

# Criar uma lista com os dois dataframes
lista_dados <- list("Treino" = treino, "Teste" = teste)

cat("Dimensões do Treino:", nrow(treino), "\n")
cat("Dimensões do Teste:", nrow(teste), "\n")

# Criar o objeto de normalização com os dados de treino
preProcValues <- preProcess(
  treino[, !names(treino) %in% "default.payment.next.month"],
  method = c("center", "scale")
)

treino_scaled <- predict(preProcValues, treino)
teste_scaled  <- predict(preProcValues, teste)
# Definir o controlo de treino (Cross-Validation)
# Usamos 10-fold cross-validation para garantir que o modelo é robusto
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
treino$default.payment.next.month <- as.factor(treino$`default payment next month`)
grid_k <- expand.grid(
  k = seq(3, 35, by = 2)
)
modelo_knn <- train(
  default.payment.next.month ~ .,
  data = treino_scaled,
  method = "knn",
  tuneGrid = grid_k,
  trControl = ctrl,
  metric = "ROC"
)

previsoes <- predict(modelo_knn, newdata = teste)
resultado <- confusionMatrix(previsoes, factor(teste$`default payment next month`))
print(resultado)
plot(modelo_knn)
modelo_knn$bestTune

# ---7. Avaliação e Fairness---
modelo_final <- train(default.payment.next.month ~ ., 
                      data = treino, 
                      method = "knn", 
                      tuneGrid = expand.grid(k = 25),
                      trControl = trainControl(method = "cv", number = 10))
previsoes <- predict(modelo_final, newdata = teste)
print(confusionMatrix(previsoes, as.factor(teste$`default payment next month`)))
prob_knn <- predict(modelo_knn, newdata = teste, type = "prob")

# Gerar a curva PR
# Nota: use a coluna correspondente à classe '1' (incumprimento)
curva_pr <- pr.curve(
  scores.class0  = prob_knn[, "Default"],
  weights.class0 = (teste$default.payment.next.month == "Default"),
  curve = TRUE
)
plot(curva_pr)
#Guardar o ficheiro em deployment
saveRDS(
  list(
    modelo = modelo_knn,
    preProcess = preProcValues,
    versao = "1.0",
    data = Sys.Date()
  ),
  "modelo_knn_m68201.rds"
)
obj <- readRDS("modelo_knn_m68201.rds")
modelo <- obj$modelo
preProc <- obj$preProcess




