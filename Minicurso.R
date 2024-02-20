# Carregando o tidyverse:
library(tidyverse)

# Carregando o gapminder:
library(gapminder)

# Filter para filtrar dados:
brazil <- filter(gapminder, country == "Brazil" & year > 1970 & year < 2000)

filter(gapminder, !(country == "Brazil" | country == "Argentina"))

# Select para filtrar colunas:
filter(select(gapminder, c(1:4)), country == "Brazil")

gapminder %>%
    select(c(1:4)) %>%
    filter(country %in% c("Brazil", "Argentina",
                          "Paraguay", "Uruguay"),
           year %in% c(1952, 2007))

# Arrange para organizar:
gapminder %>%
    select(c(1:4)) %>%
    filter(country == "Brazil" | country == "Argentina") %>%
    arrange(desc(year))

# Mutate para transformações:
gapminder %>%
    mutate(Multiplicacao = year * lifeExp) %>%
    filter(Multiplicacao %% 2 == 0)


iris %>%
    tibble() %>%
    select(c(Petal.Length, Sepal.Length, Species)) %>%
    transmute(Petala = Petal.Length,
              Sepala = Sepal.Length,
              Especie = Species) %>%
    transmute(Soma = Petala + Sepala,
              Multiplicacao = Petala * Sepala,
              Soma_Maior = ifelse(Soma < 0.8 * Multiplicacao, 1, 0))

# Group_by para agrupar:
gapminder %>%
    group_by(continent)

# Summarise para criar sumários
gapminder %>%
    summarise(Total = n())

# Juntos:
gapminder %>%
    group_by(continent, year) %>%
    summarise(Soma = sum(pop)) %>%
    mutate(Total_Total = sum(Soma),
           Percentual = 100 * Soma/Total_Total) %>%
    select(continent, year, Percentual) %>%
    pivot_wider(names_from = year, values_from = Percentual)

# tidyr::pivot
gapminder %>%
    group_by(continent, year) %>%
    summarise(Pop_Tot = sum(pop)) %>%
    pivot_wider(names_from = year, values_from = Pop_Tot)

# ggplot2 para gráficos:
titanic <- as_tibble(Titanic)

gapminder %>%
    group_by(continent, year) %>%
    summarise(Pop_Media = mean(pop)) %>%
    ggplot(aes(x = year, y = Pop_Media,
               group = continent, color = continent)) +
    geom_point() +
    geom_line()

gapminder %>%
    ggplot(aes(x = year, y = pop, group = year)) +
    geom_boxplot() +
    facet_wrap(~continent) +
    lims(y = c(0, 250000000))

mpg %>%
    group_by(manufacturer, year, cyl) %>%
    summarise(Rend_Med = mean(cty)) %>%
    ggplot(aes(x = year, y = Rend_Med, group = manufacturer)) +
    geom_point() +
    geom_line() +
    facet_grid(~cyl)

titanic %>%
    ggplot(aes(x = Class, y = n,
               color = Age)) +
    geom_point() +
    facet_grid(rows = vars(Survived),
               cols = vars(Sex)) +
    theme_bw()


# TODO Links de leituras recomendadas
