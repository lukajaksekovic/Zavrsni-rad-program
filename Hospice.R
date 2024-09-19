library(simmer)
library(ggplot2)
library(dplyr)


# Definiranje parametara za simulaciju

broj_sestara <- 1
broj_lijecnika <- 1
prosjek_stopa_dolaska <- 5
prosjek_vrijeme_prijema <- 5
prosjek_vrijeme_pregleda <- 10
vrijeme_simulacije <- 480


# Definiranje putanje pacijenta

putanja_pacijenta <- trajectory("Putanja pacijenta") %>%
  seize("sestra", 1) %>%
  timeout(function() rnorm(1, prosjek_vrijeme_prijema, 0.5)) %>%
  release("sestra", 1) %>%
  seize("lijecnik", 1) %>%
  timeout(function() rnorm(1, prosjek_vrijeme_pregleda, 1)) %>%
  release("lijecnik", 1)


# Stvaranje simulacijskog okruženja

hospicij <- simmer("Hospicij") %>%
  add_resource("sestra", broj_sestara) %>%
  add_resource("lijecnik", broj_lijecnika) %>%
  add_generator("Pacijent", putanja_pacijenta, function() rexp(1, 1 / prosjek_stopa_dolaska))


# Pokrećemo simulaciju

hospicij %>%
  run(vrijeme_simulacije)


# Dohvaćanje podataka resursa simulacije

podaci_o_resursima <- get_mon_resources(hospicij)


# Filter za prikaz samo duljinu reda

podaci_o_redu <- podaci_o_resursima %>%
  select(time, resource, queue) %>%
  rename(duzina_reda = queue) %>%
  filter(duzina_reda > 0)


# Namještanje vrijednosti za osi x i y

x_intervali <- seq(0, max(podaci_o_redu$time, na.rm = TRUE), by = 20)
y_intervali <- seq(0, max(podaci_o_redu$duzina_reda, na.rm = TRUE), by = 2)


# Grafički prikaz duljine reda kroz vrijeme

ggplot(podaci_o_redu, aes(x = time, y = duzina_reda, color = resource)) +
  geom_line(size = 1) +
  labs(
    title = "Duljina reda kroz vrijeme",
    x = "Vrijeme (minute)",
    y = "Duljina reda",
    color = "Resurs"
  ) +
  scale_x_continuous(breaks = x_intervali, labels = scales::label_number()) +
  scale_y_continuous(breaks = y_intervali, labels = scales::label_number()) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1),  # Rotacija oznaka osi x radi bolje čitljivosti
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )