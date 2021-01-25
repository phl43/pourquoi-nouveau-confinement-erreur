library(tidyverse)
library(lubridate)
library(qualpalr)
library(data.table)
library(zoo)
library(googleLanguageR)
library(stringr)
library(httr)
library(utils)
library(readxl)
library(haven)
library(ggrepel)
library(cowplot)

# il vous faut votre propre clé pour utiliser l'API de Google
gl_auth("api_authentication.json")

owid_data_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

owid_data <- fread(owid_data_url) %>%
  as_tibble() %>%
  filter(date <= ymd("2021-01-22")) %>% # il manque des données après le 22 janvier
  mutate(
    date = ymd(date),
    cases = replace_na(new_cases, 0),
    new_cases_smoothed = round(rollmean(cases, 7, fill = c(0, 0, 0), align = "right")),
    deaths = replace_na(new_deaths, 0),
    new_deaths_smoothed = round(rollmean(deaths, 7, fill = c(0, 0, 0), align = "right")),
    new_deaths_smoothed_per_million = replace_na(new_deaths_smoothed_per_million, 0),
    total_deaths_per_million = replace_na(total_deaths_per_million, 0),
    total_deaths = replace_na(total_deaths, 0)
  )

country_names_translation <- gl_translate(unique(owid_data$location), target = "fr") %>%
  mutate(
    translatedText = str_replace_all(
      translatedText,
      "((La|la|Le|le)\\s+|L'|l')",
      ""
    )
  )

owid_data <- owid_data %>%
  rowwise() %>%
  mutate(
    country = country_names_translation$translatedText[country_names_translation$text == location]
  ) %>%
  ungroup() %>%
  select(
    date,
    continent,
    country,
    new_cases_smoothed,
    new_deaths_smoothed,
    new_deaths_smoothed_per_million,
    total_deaths_per_million,
    total_deaths,
    population
  )

# make sure the data start on 31 December 2019 for every country
for (c in unique(owid_data$country)) {
  owid_data <- owid_data %>%
    bind_rows(
      tibble(
        date = seq(ymd("2019-12-31"), min(owid_data$date[owid_data$country == c]) - 1, by = "1 day"),
        country = c,
        new_deaths_smoothed_per_million = rep(0, min(owid_data$date[owid_data$country == c]) - ymd("2019-12-31")),
        total_deaths_per_million = rep(0, min(owid_data$date[owid_data$country == c]) - ymd("2019-12-31"))
      )
    )
}

non_eu <- c(
  "Albanie",
  "Andorre",
  "Biélorussie",
  "Bosnie Herzégovine",
  "Islande",
  "Kosovo",
  "Liechtenstein",
  "Moldavie",
  "Monaco",
  "Monténégro",
  "Macédoine du Nord",
  "Norvège",
  "Russie",
  "Saint Marin",
  "Serbie",
  "Suisse",
  "Ukraine",
  "Vatican"
)

owid_data_eu <- owid_data %>%
  filter(continent == "Europe" & !(country %in% non_eu) & date >= ymd("2020-03-15"))

comparison_sweden_eu <- owid_data_eu %>%
  mutate(sweden = country == "Suède") %>%
  group_by(date, sweden) %>%
  summarize(
    total_deaths_per_million = sum(total_deaths) / sum(population) * 1e6
  ) %>%
  ungroup() %>%
  mutate(
    location = ifelse(sweden, "Suède", "UE")
  ) %>%
  select(
    date,
    location,
    total_deaths_per_million
  )

ggplot(comparison_sweden_eu, aes(x = date, y = total_deaths_per_million, group = location, color = location)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle(
    "Nombre total de morts du COVID-19 par million d'habitant en Suède et dans l'UE sans la Suède",
    subtitle = "(la chute du 4 avril pour la Suède est due à une correction dans les données ce jour-là)"
  ) +
  xlab("Date") +
  ylab("Nombre total de morts du COVID-19 par million d'habitant") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "7 day"
  ) +
  scale_y_continuous(trans = "log10") +
  scale_color_discrete(name = "Pays") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(caption = "Source : Our World in Data - Graphique par Philippe Lemoine (@phl43)") +
  ggsave("Nombre total de morts du COVID-19 par million d'habitant en Suède et dans l'UE sans la Suède.png", width = 12, height = 6)


owid_data_eu <- owid_data_eu %>%
  mutate(
    ratio_se = total_deaths_per_million / total_deaths_per_million[country == "Suède"]
  ) %>%
  ungroup()

set.seed(46)

n_countries <- length(unique(owid_data_eu$country))

color_data <- data.frame(
  team = replicate(n_countries, paste(sample(LETTERS, 4), collapse = "")),
  score = rnorm(n_countries, 90, 10),
  skill = runif(n_countries, 1, 10)
)

# Generate color palette
palette <- qualpal(
  n_countries,
  colorspace = list(h = c(0,360), s = c(0.3,1), l = c(0.2,0.8))
)

deaths_plot_eu <- ggplot(owid_data_eu, aes(x = date, y = total_deaths_per_million, group = country, color = country)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Nombre de morts du COVID-19 par million d'habitants dans l'UE") +
  xlab("Date") +
  ylab("Nombre de morts du COVID-19 par million d'habitants") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "7 day"
  ) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(
    name = "Pays",
    values = palette$hex
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Our World in Data - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre de morts du COVID-19 par million d'habitants dans l'UE - 1.png", width = 12, height = 6)

deaths_plot_eu_se <- ggplot(owid_data_eu %>% filter(date >= ymd("2020-10-01")), aes(x = date, y = ratio_se, group = country, color = country)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Nombre de morts du COVID-19 par million d'habitants dans l'UE par rapport à la Suède") +
  xlab("Date") +
  ylab("Nombre de morts du COVID-19 par million d'habitants en proportion de la Suède") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "7 day"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    name = "Pays",
    values = palette$hex
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Our World in Data - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre de morts du COVID-19 par million d'habitants dans l'UE par rapport à la Suède.png", width = 12, height = 6)

plot_grid(
  deaths_plot_eu,
  deaths_plot_eu_se,
  labels = c("", ""),
  ncol = 1
) +
  ggsave("Convergence de la mortalité dans les pays de l'UE par rapport à la Suède.png", width = 12, height = 12)

url_données_tests <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"

incidence_france_seconde_vague <- read_delim(url_données_tests, delim = ";") %>%
  filter(cl_age90 == "0") %>%
  mutate(
    date = ymd(jour)
  ) %>%
  group_by(date) %>%
  summarize(
    cas = sum(P)
  ) %>%
  mutate(
    moyenne_mobile_cas = round(rollmean(cas, 7, fill = c(0, 0, 0), align = "right"))
  ) %>%
  filter(date >= ymd("2020-10-01") & date <= ymd("2020-12-24")) %>%
  select(
    date,
    cas,
    moyenne_mobile_cas
  )

ggplot(incidence_france_seconde_vague, aes(x = date, y = moyenne_mobile_cas)) +
  geom_line(size = 1, color = "steelblue") +
  geom_vline(aes(xintercept = ymd("2020-10-17"), linetype = "couvre-feu1"), color = "darkgreen", size = 1) +
  geom_vline(aes(xintercept = ymd("2020-10-24"), linetype = "couvre-feu2"), color = "purple", size = 1) +
  geom_vline(aes(xintercept = ymd("2020-10-30"), linetype = "confinement"), color = "orange", size = 1) +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en France lors de la seconde vague (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "7 day"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "couvre-feu1" = 2,
      "couvre-feu2" = 2,
      "confinement" = 2
    ),
    labels = c(
      "Couvre-feu en IDF et\ndans 8 métropoles",
      "Couvre-feu étendu à\n54 départements",
      "Confinement"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("darkgreen", "purple", "orange"))
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : Santé publique France - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en France lors de la seconde vague.png", width = 12, height = 6)

incidence_israel <- owid_data %>%
  filter(country == "Israël" & date >= ymd("2020-12-15") & date <= ymd("2021-01-22"))

ggplot(incidence_israel, aes(x = date, y = new_cases_smoothed)) +
  geom_line(size = 1, color = "steelblue") +
  geom_vline(aes(xintercept = ymd("2020-12-27"), linetype = "confinement"), color = "orange", size = 1) +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en Israël lors de la troisième vague (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "7 day"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c("confinement" = 2),
    labels = c("Confinement"),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("orange"))
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : Our World in Data - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Israël lors de la troisième vague.png", width = 12, height = 6)

data_url <- "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data"

GET(
  data_url,
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

data_icu_sweden <- read_excel(
  tf,
  sheet = "Antal intensivvårdade per dag"
) %>%
  rename(
    date = Datum_vårdstart,
    icu = Antal_intensivvårdade
  ) %>%
  mutate(
    date = as_date(date),
    rolling_average_icu = round(rollmean(icu, 7, fill = c(0, 0, 0), align = "right"))
  )

data_cases_sweden <- read_excel(
  tf,
  sheet = "Antal per dag region"
) %>%
  rename(
    date = Statistikdatum,
    cases = Totalt_antal_fall
  ) %>%
  mutate(
    date = as_date(date),
    rolling_average_cases = round(rollmean(cases, 7, fill = c(0, 0, 0), align = "right"))
  ) %>%
  filter(date >= min(data_icu_sweden$date))

cases_plot_sweden <- ggplot(data_cases_sweden, aes(x = date, y = rolling_average_cases)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en Suède (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_y_continuous() +
  scale_x_date(
    labels = scales::date_format("%Y/%m/%d"),
    date_breaks = "7 day"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Folkhälsomyndigheten - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Suède.png", width = 12, height = 6)

icu_plot_sweden <- ggplot(data_icu_sweden, aes(x = date, y = rolling_average_icu)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien d'admissions en réanimation pour COVID-19 en Suède (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien d'admissions en réanimation pour COVID-19") +
  scale_x_date(
    labels = scales::date_format("%Y/%m/%d"),
    date_breaks = "7 day"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Folkhälsomyndigheten - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien d'admissions en réanimation pour COVID-19 en Suède.png", width = 12, height = 6)

plot_grid(
  cases_plot_sweden,
  icu_plot_sweden,
  labels = c("", ""),
  ncol = 1
) +
  ggsave("Nombre quotidien de cas de COVID-19 et d'admissions en réanimation pour COVID-19 en Suède.png", width = 12, height = 12)

incidence_serbia <- owid_data %>%
  filter(country == "Serbie" & date >= ymd("2020-10-15"))

ggplot(incidence_serbia, aes(x = date, y = new_cases_smoothed)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en Serbie (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : Our World in Data - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Serbie.png", width = 12, height = 6)

url_data_us <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

data_us <- fread(url_data_us) %>%
  as_tibble() %>%
  group_by(date, state) %>%
  summarize(
    cases = sum(cases),
    deaths = sum(deaths)
  ) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(
    new_cases = cases - lag(cases, default = 0),
    new_deaths = deaths - lag(deaths, default = 0),
    new_cases_smoothed = round(rollmean(new_cases, 7, fill = c(0, 0, 0), align = "right")),
    new_deaths_smoothed = round(rollmean(new_deaths, 7, fill = c(0, 0, 0), align = "right"))
  ) %>%
  ungroup()

data_florida <- data_us %>%
  filter(state == "Florida")

cases_plot_florida <- ggplot(data_florida, aes(x = date, y = new_cases_smoothed)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en Floride (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : New York Times - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Floride.png", width = 12, height = 6)

deaths_plot_florida <- ggplot(data_florida, aes(x = date, y = new_deaths_smoothed)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien de décès attribués au COVID-19 en Floride (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de décès attribués au COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : New York Times - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Floride.png", width = 12, height = 6)

plot_grid(
  cases_plot_florida,
  deaths_plot_florida,
  labels = c("", ""),
  ncol = 1
) +
  ggsave("Nombre quotidien de cas de COVID-19 et de décès attribués au COVID-19 en Floride.png", width = 12, height = 12)

population_density_data <- read_dta("pop_weighted_density_xcountry.dta") %>%
  rename(
    country = name,
    pwd = hhi
  ) %>%
  mutate(
    country = str_replace_all(
      gl_translate(country, target = "fr")$translatedText,
      "((La|la|Le|le)\\s+|L'|l')",
      ""
    )
  ) %>%
  select(country, pwd)

microstates <- c(
  "Andorre",
  "Gibraltar",
  "Guernsey",
  "Île de Man",
  "Jersey",
  "Liechtenstein",
  "Monaco",
  "Saint Marin"
)

owid_data_europe <- owid_data %>%
  filter(continent == "Europe" & !(country %in% microstates))

data_covid_density <- inner_join(owid_data_europe, population_density_data, by = "country") %>%
  filter(date == ymd("2021-01-22")) %>%
  select(
    country,
    total_deaths_per_million,
    pwd
  )

fit <- summary(lm(log(total_deaths_per_million) ~ log(pwd), data_covid_density))

ggplot(data_covid_density, aes(x = pwd, y = total_deaths_per_million)) +
  geom_point() +
  geom_text_repel(aes(label = country), max.overlaps = 15) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_text(
    aes(x = 975, y = 1600),
    label = sprintf(
      "log(Y) = %.2f + %.2flog(x)\nP = %.2f\nR\UB2 = %.2f",
      round(fit$coefficients[1,1], 2),
      round(fit$coefficients[2,1], 2),
      round(fit$coefficients[2,4], 2),
      round(fit$r.squared, 2)
    )
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  ggtitle("Nombre de morts du COVID-19 par million d'habitants vs. densité de population pondérée par la population en Europe") +
  xlab("Densité de population") +
  ylab("Nombre de morts du COVID-19 par million d'habitants") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Our World in Data pour les morts du COVID-19 et Antoine Lévy (@LevyAntoine) pour la densité de population - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre de morts du COVID-19 par million d'habitants vs. densité de population pondérée par la population en Europe.png", width = 12, height = 6)


incidence_spain <- owid_data %>%
  filter(country == "Espagne" & date >= ymd("2020-03-01"))

ggplot(incidence_spain, aes(x = date, y = new_cases_smoothed)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Nombre quotidien de cas de COVID-19 en Espagne (moyenne mobile sur 7 jours)") +
  xlab("Date") +
  ylab("Nombre quotidien de cas de COVID-19") +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "15 day"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(caption = "Source : Our World in Data - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Nombre quotidien de cas de COVID-19 en Espagne.png", width = 12, height = 6)
