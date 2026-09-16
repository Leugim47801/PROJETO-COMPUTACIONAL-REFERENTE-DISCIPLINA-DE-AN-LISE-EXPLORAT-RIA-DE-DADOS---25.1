
#===================== Carregando a base de dados==========================
spotify <- read.csv("https://github.com/HugoCarvalhoUFRJ/aed/raw/refs/heads/main/materiais-didaticos/spotify-2023-mod.csv", header = TRUE, sep = ',', dec = '.')
#==================== Instalando bibliotecas/pacote =======================
install.packages('corrplot')
install.packages('patchwork')
library(corrplot)
library(tidyverse)
library(ggplot2)
library(patchwork)
# ======================= Resumo da Base de dados =========================
str(spotify)
#======= Transformando as variáveis para melhor interpretação =============
spotify$artist_count <- as.factor(spotify$artist_count)
spotify$streams <- as.integer(spotify$streams)
spotify$in_deezer_playlist <- as.integer(spotify$in_deezer_playlist)
spotify$in_shazam_charts <- as.integer(spotify$in_shazam_charts)
spotify$mode <- as.factor(spotify$mode)


meses <- c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')
spotify$realeased_month <- factor(spotify$released_month, labels = meses, ordered = TRUE)
head(spotify)
# =============== Verificando mudanças na base de dados ==================
str(spotify)
# =============== Verificar os dados faltantes ===========================
# Usando sapply para contar NA, "" e " " em cada coluna
missing_counts <- sapply(spotify, function(col) {sum(is.na(col) | col == "" | col == " ")})
# Criar um data.frame com os resultados
missing_data <- data.frame(Coluna = names(missing_counts), Valores_Ausentes = missing_counts, row.names = NULL)
# Ordenar do maior para o menor
missing_data <- missing_data[order(missing_data$Valores_Ausentes, decreasing = TRUE), ]
# Visualizar o resultado
print(missing_data)
#====================== Gráfico dos dados ausentes ========================
library(ggplot2)
# Pegar as top 5 colunas com mais missing values
top_missing <- head(missing_data, 4)
# Criar o gráfico
ggplot(top_missing, aes(x = reorder(Coluna, -Valores_Ausentes), y = Valores_Ausentes)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(title = "Contagem dos dados ausentes no arquivo 'spotify-2023.csv", x = "Coluna", y = "Contagem de Valores Ausentes") +
  theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
#=============== Gráfico de correlação entre as variáveis =================
cols = c('streams', 'bpm', 'danceability_.', 'valence_.', 'energy_.',
'acousticness_.', 'instrumentalness_.', 'liveness_.', 'speechiness_.')
corrplot(cor(spotify[, cols], use = 'complete.obs'), method = 'number', main = "Correlação entre os dados 'spotify-2023.csv'", mar=c(0,0,1,0), number.cex = 0.76, tl.cex = 0.8)
# —---------------Analisando os dados com  streams —-----------------------
cols = c('streams',"in_spotify_playlists", "in_spotify_charts", "in_apple_playlists", "in_apple_charts", "in_deezer_charts")
corrplot(cor(spotify[, cols], use = 'complete.obs'), method = 'number', main = "Correlação entre os dados 'spotify-2023.csv'- 2", mar=c(0,0,1,0), number.cex = 1.5, tl.cex = 1 )
# ====== Outros gráficos de correlação entre variáveis técnicas =========
library(ggplot2)
# Gráfico entre streams vs. danceability_.
p1 <- ggplot(spotify, aes(x = danceability_., y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs. Danceability",
       x = "Danceability (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. energy_.
p2 <- ggplot(spotify, aes(x = energy_., y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs. Energy",
       x = "Energy (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. valence_.
p3 <- ggplot(spotify, aes(x = valence_., y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs. Valence",
       x = "Valence (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. bpm_.
p4 <- ggplot(spotify, aes(x = bpm, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs bpm",
       x = "Bpm (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs acousticness.
p5 <- ggplot(spotify, aes(x = acousticness_., y = streams),na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs Acousticness",
       x = " Acousticness (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs instrumentalness.
p6 <- ggplot(spotify, aes(x = instrumentalness_., y = streams),na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs Instrumentalnes",
       x = "Instrumentalnes (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs liveness.
p7 <- ggplot(spotify, aes(x = liveness_., y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs Liveness",
       x = "Liveness (%)",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs speechiness.
p8 <- ggplot(spotify, aes(x = speechiness_., y = streams, na.rm = TRUE)) +
  geom_point(alpha = 0.5) +
  labs(title = "Streams vs Speechiness",
       x = "Speechiness (%)",
       y = "Streams") +
  theme_minimal()
# Demonstrar os gráficos
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(6)
print(7)
print(8)
# ================ Gráficos entre variáveis de “charts” ===================
library(ggplot2)
library(patchwork) 
# Gráfico entre streams vs. in_spotify_playlists
p9 <- ggplot(spotify, aes(x = in_spotify_playlists, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs. Spotify Playlists",
       x = "Number of Spotify Playlists",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. in_spotify_charts
p10 <- ggplot(spotify, aes(x = in_spotify_charts, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs. Spotify Charts",
       x = "Number of Spotify Charts",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. in_apple_playlists
p11 <- ggplot(spotify, aes(x = in_apple_playlists, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs. Apple Playlists",
       x = "Number of Apple Playlists",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. in_apple_charts
p12 <- ggplot(spotify, aes(x = in_apple_charts, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs. Apple Charts",
       x = "Number of Apple Charts",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. in_deezer_playlists
p13 <- ggplot(spotify, aes(x = in_deezer_playlists, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs Playlists no Deezer",
       x = "Número de Playlists no Deezer",
       y = "Streams") +
  theme_minimal()
# Gráfico entre streams vs. in_deezer_charts
p14 <- ggplot(spotify, aes(x = in_deezer_charts, y = streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Streams vs Charts do Deezer",
       x = "Número no Charts do Deezer",
       y = "Streams") +
  theme_minimal()
# Demonstrar os gráficos
print(p9)
print(p10)
print(p11)
print(p12)
print(p13)
print(p14)


cols = c('streams', 'bpm', 'danceability_.', 'valence_.', 'energy_.',
'acousticness_.', 'instrumentalness_.', 'liveness_.', 'speechiness_.', "in_spotify_playlists", "in_spotify_charts", "in_apple_playlists", "in_apple_charts", "in_deezer_charts")
corrplot(cor(spotify[, cols], use = 'complete.obs'), method = 'number', main = "Correlação entre os dados 'spotify-2023.csv'- Completo", mar=c(0,0,1,0), number.cex = 0.76, tl.cex = 0.8)
# =================== acoustiness com energy ==============================
spotify <- na.omit(spotify)
 grafico <- ggplot(spotify, aes(x =spotify$acousticness_. , y =spotify$energy_. )) +
 geom_col(fill = 'violet') +
 ggtitle('Acousticness por Energy ') +
 labs(x = "Acousticness", y = 'Energy')
 print(grafico)
# acustico
 media_acoustic <- mean(spotify$acousticness, na.rm = TRUE)
 mediana_acoustic <- median(spotify$acousticness, na.rm = TRUE)
 dp_acoustic <- sd(spotify$acousticness, na.rm = TRUE)
 cat("Estatísticas para a variável 'Acousticness':\n")
 cat(paste("Média:", round(media_acoustic, 2), "\n"))
 cat(paste("Mediana:", mediana_acoustic, "\n"))
 cat(paste("Desvio Padrão:", round(dp_acoustic, 2), "\n"))
# nrg
media_nrg <- mean(spotify$valence_, na.rm = TRUE)
mediana_nrg <- median(spotify$energy_, na.rm = TRUE)
dp_nrg <- sd(spotify$energy_, na.rm = TRUE)
cat("Estatísticas para a variável 'Energy':\n")
cat(paste("Média:", round(media_nrg, 2), "\n"))
cat(paste("Mediana:", mediana_nrg, "\n"))
cat(paste("Desvio Padrão:", round(dp_nrg, 2), "\n"))
#=================== dance e valence ======================================
spotify <- na.omit(spotify)
grafico <- ggplot(spotify, aes(x = danceability_., y = valence_.)) + geom_col(fill = 'red') +
ggtitle('Danceability por valence') +
labs(x = 'Danceability', y = 'Valence')
print(grafico)
# dance
 media_dance <- mean(spotify$danceability_, na.rm = TRUE)
 mediana_dance <- median(spotify$danceability_, na.rm = TRUE)
 dp_dance <- sd(spotify$danceability_, na.rm = TRUE)
 cat("Estatísticas para a variável 'danceability':\n")
 cat(paste("Média:", round(media_dance, 2), "\n"))
 cat(paste("Mediana:", mediana_dance, "\n"))
 cat(paste("Desvio Padrão:", round(dp_dance, 2), "\n"))
# valence
 media_valence <- mean(spotify$valence_, na.rm = TRUE)
 mediana_valence <- median(spotify$valence_, na.rm = TRUE)
 dp_valence <- sd(spotify$valence_, na.rm = TRUE)
 cat("Estatísticas para a variável 'Valence':\n")
 cat(paste("Média:", round(media_valence, 2), "\n"))
 cat(paste("Mediana:", mediana_valence, "\n"))
 cat(paste("Desvio Padrão:", round(dp_valence, 2), "\n"))


#===================== nrg e valence ======================================
spotify <- na.omit(spotify)
grafico <- ggplot(spotify, aes(x = energy_., y = valence_.)) + geom_col(fill = 'red') +
ggtitle('Energy por valence') +
labs(x = 'Energy', y = 'Valence')
print(grafico)


#====================== GRÁFICO STREAM E PLAYLIST =========================
library(ggplot2)
library(patchwork)
p_spotify <- ggplot(streams_by_all_playlists, aes(x = in_spotify_playlists, y = total_streams), na.rm=TRUE) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Streams vs. Spotify Playlists",
       x = "Number of Spotify Playlists",
       y = "Total Streams") +
  theme_minimal()
streams_by_all_playlists$in_deezer_playlists_numeric <- as.numeric(gsub(",", "", streams_by_all_playlists$in_deezer_playlists), na.rm=TRUE)
p_deezer <- ggplot(streams_by_all_playlists, aes(x = in_deezer_playlists_numeric, y = total_streams), na.rm = TRUE) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Streams vs. Deezer Playlists",
       x = "Number of Deezer Playlists",
       y = "Total Streams") +
  theme_minimal()
p_apple <- ggplot(streams_by_all_playlists, aes(x = in_apple_playlists, y = total_streams), na.rm=TRUE) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Streams vs. Apple Playlists",
       x = "Number of Apple Playlists",
       y = "Total Streams") +
  theme_minimal()
(p_spotify + p_deezer) / p_apple
