library(tweedie)
setwd("D:/Actuariat/6e session/stats/tp/rapport 2")
### Importer les données et préparer train/eval

load("imp_data.RData")
set.seed(536778899, sample.kind = "Rejection")
ind.entr <- sample(1:nrow(completedata), 0.85*nrow(completedata), FALSE)
train <- completedata[ind.entr,]
test <- completedata[-ind.entr,]

ind.run <- sample(1:nrow(completedata), 1000, FALSE)
run <- completedata[ind.run,]


### GLM Tweedie
library(tweedie)
library(statmod)
#tweed.p <- tweedie.profile(ClaimAmount ~ Gender +  DrivAge + VehYear + Area + State + VehManuf + SumInsAvg,
#                          data = train,
#                         p.vec = seq(1.05, 1.95, 0.05),
#                        method="series", do.plot=TRUE)$p.max

tweed.p <- 1.60102
glm_tweed <- glm(ClaimAmount ~ Gender +  DrivAge + VehYear + Area + State + VehManuf + SumInsAvg + offset(I(log(ExposTotal))),
                 data = train,
                 family = tweedie(var.power = tweed.p, link.power = 0))

preds <- predict(glm_tweed, test,type = "response")

sum(test$ClaimAmount)/sum(preds)

null_dev <- sum((test$ClaimAmount - mean(test$ClaimAmount))^2)
full_dev <- sum((test$ClaimAmount - preds)^2)
dev_explained <- 1 - full_dev / null_dev


# Chargement des bibliothèques
library(ggplot2)
library(scales)
test <- test[test$PremTotal>10,]
# 1. Créer les primes
P_comp <- predict(glm_tweed, newdata = test, type = "response") # Notre prime à comparer est la Tweedie
P_ref <- test$PremTotal # Notre prime de référence est la prime dans le champ PremTotal

# 2. Relativité
r <- P_comp / P_ref


## 3. Trier les individus
sort_id2 <- order(r)

## 4. On crée 5 groupes qui contiennent la même exposition (environ)
expo_cumul2 <- cumsum(test$ExposTotal[sort_id2])
cut_points2 <- sum(test$ExposTotal) / 5 * 0:5
groups2 <- cut(expo_cumul2, cut_points2, labels = FALSE)

lower_bounds <- c(r[sort_id2[1]], r[sort_id2[which(diff(groups2) > 0)]])
upper_bounds <- c(r[sort_id2[which(diff(groups2) > 0) - 1]], r[sort_id2[length(sort_id2)]])

## 5. On calcule le Loss Ratio par groupe
test_sort2 <- test[sort_id2, ]
test_sort2$r_group <- groups2
test_sort2$P_ref <- P_ref[sort_id2]
LossRatio_table2 <- aggregate(cbind(P_ref, ClaimAmount) ~ r_group, data = test_sort2, FUN = sum)
LossRatio_table2$LR_ref <- LossRatio_table2$ClaimAmount / LossRatio_table2$P_ref


# Ajouter les bornes inférieures et supérieures au tableau LossRatio_table2
LossRatio_table2$lower_bound <- lower_bounds
LossRatio_table2$upper_bound <- upper_bounds


library(ggplot2)
# Récupérer les coordonnées du milieu de la première et de la dernière bande
y_start <- LossRatio_table2$LR_ref[1]
y_end <- LossRatio_table2$LR_ref[nrow(LossRatio_table2)]
x_start <- 1
x_end <- nrow(LossRatio_table2)

slope <- (y_end - y_start) / (x_end - x_start)

r[sort_id2[which(diff(groups2) > 0)]]



# Tracer le graphique avec les intervalles de ratio ajoutés
ggplot(LossRatio_table2, aes(x = r_group, y = LR_ref, fill = LR_ref)) +
    geom_bar(stat = "identity") +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "darkred", linetype = "solid", size = 1.2) +
    geom_point(aes(x = x_start, y = y_start), color = "darkred", size = 3) +
    geom_point(aes(x = x_end, y = y_end), color = "darkred", size = 3) +
    annotate("text", x = 1, y = max(LossRatio_table2$LR_ref), label = paste0("Pente : ", round(slope, 4)), hjust = 0, vjust = 1, size = 4, color = "darkred") +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Groupes de relativité", y = "Loss Ratio",
         caption = "Tweedie à comparer et prime brésilienne comme référence") +
    theme(legend.position = "none") +
    # Ajouter les intervalles de ratio sous chaque barre
    geom_text(aes(x = r_group, y = 0,
                  label = paste0("(", round(lower_bound, 2), ", ", round(upper_bound, 2), ")")),
              vjust = 1.5, size = 3, angle = 0)
####################


# 1. Créer les primes
P_ref<- predict(glm_tweed, newdata = test, type = "response") # Notre prime à comparer est la Tweedie
P_comp <- test$PremTotal # Notre prime de référence est la prime dans le champ PremTotal

# 2. Relativité
r <- P_comp / P_ref


## 3. Trier les individus
sort_id2 <- order(r)

## 4. On crée 5 groupes qui contiennent la même exposition (environ)
expo_cumul2 <- cumsum(test$ExposTotal[sort_id2])
cut_points2 <- sum(test$ExposTotal) / 5 * 0:5
groups2 <- cut(expo_cumul2, cut_points2, labels = FALSE)

lower_bounds <- c(r[sort_id2[1]], r[sort_id2[which(diff(groups2) > 0)]])
upper_bounds <- c(r[sort_id2[which(diff(groups2) > 0) - 1]], r[sort_id2[length(sort_id2)]])

## 5. On calcule le Loss Ratio par groupe
test_sort2 <- test[sort_id2, ]
test_sort2$r_group <- groups2
test_sort2$P_ref <- P_ref[sort_id2]
LossRatio_table2 <- aggregate(cbind(P_ref, ClaimAmount) ~ r_group, data = test_sort2, FUN = sum)
LossRatio_table2$LR_ref <- LossRatio_table2$ClaimAmount / LossRatio_table2$P_ref


# Ajouter les bornes inférieures et supérieures au tableau LossRatio_table2
LossRatio_table2$lower_bound <- lower_bounds
LossRatio_table2$upper_bound <- upper_bounds


library(ggplot2)
# Récupérer les coordonnées du milieu de la première et de la dernière bande
y_start <- LossRatio_table2$LR_ref[1]
y_end <- LossRatio_table2$LR_ref[nrow(LossRatio_table2)]
x_start <- 1
x_end <- nrow(LossRatio_table2)

slope <- (y_end - y_start) / (x_end - x_start)

r[sort_id2[which(diff(groups2) > 0)]]



# Tracer le graphique avec les intervalles de ratio ajoutés
ggplot(LossRatio_table2, aes(x = r_group, y = LR_ref, fill = LR_ref)) +
    geom_bar(stat = "identity") +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "darkred", linetype = "solid", size = 1.2) +
    geom_point(aes(x = x_start, y = y_start), color = "darkred", size = 3) +
    geom_point(aes(x = x_end, y = y_end), color = "darkred", size = 3) +
    annotate("text", x = 1, y = max(LossRatio_table2$LR_ref), label = paste0("Pente : ", round(slope, 4)), hjust = 0, vjust = 1, size = 4, color = "darkred") +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Groupes de relativité", y = "Loss Ratio",
         caption = "prime brésilienne à comparer et Tweedie comme référence") +
    theme(legend.position = "none") +
    # Ajouter les intervalles de ratio sous chaque barre
    geom_text(aes(x = r_group, y = 0,
                  label = paste0("(", round(lower_bound, 2), ", ", round(upper_bound, 2), ")")),
              vjust = 1.5, size = 3, angle = 0)

