# O pacote essencial para todo cientista de dados:
library(tidyverse)

# Base com os participantes:
participantes <- read_xlsx("Documento")

letras <- as_tibble(letters) %>%
    select(Letra = value)

# Seed temporal:
set.seed(tempo <- as.numeric(Sys.time()))

# Sorteio dos Livros

# Primeiro sorteio:
(s_1 <- sample(letras$Letra, 1))

letras <- letras %>%
    filter(Letra != s_1)

# Segundo sorteio:
(s_2 <- sample(letras$Letra, 1))

# Sorteio das canecas:
letras <- as_tibble(letters) %>%
    select(Letra = value)

# Separando memória:
sorteados <- character(14)

for (i in 1:14) {
    sorteados[i] <- sample(letras$Letra, 1)
    letras <- letras %>%
        filter(Letra != sorteados[i])
}
