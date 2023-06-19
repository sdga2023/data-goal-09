library(dplyr)
library(tidyr)
library(tibble)
library(wbstats)
library(WDI)
library(readxl)
library(countrycode)

countries <- wb_countries()

# Air traffic
air.world <- read_excel("../input/update air traffic .xlsx", sheet = "global")
air.world <- as.data.frame(t(air.world))
air.world <- rownames_to_column(air.world)
colnames(air.world) <- c("year", "freight", "passengers", "departures")
air.world <- filter(air.world, year != "...1") %>%
  mutate(year = round(as.numeric(year))) %>%
  mutate(iso3c = "WLD") %>%
  select(iso3c, year, passengers, freight, departures)

air.countries <- read_excel("../input/update air traffic .xlsx", sheet = "country") %>%
  pivot_longer(cols = 3:51, names_to = "year", values_to = "value") %>%
  mutate(value = round(value)) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = Series_Code, values_from = value) %>%
  mutate(year = gsub('YR', '', year)) %>%
  select(iso3c = Country_Code, year, passengers = IS.AIR.PSGR, freight = IS.AIR.GOOD.MT.K1, departures = IS.AIR.DPRT) %>%
  filter(iso3c != "SWE")

air.ok <- rbind(air.world, air.countries)
air.ok <- arrange(air.ok, iso3c, year)

write.csv(air.ok, file = "../output/airtraffic.csv", row.names = FALSE)

# Containers
containers <- WDI(
  country = '1W',
  indicator=c('IS.SHP.GOOD.TU'),
  extra=TRUE
)
containers.ok <- filter(containers, !is.na(IS.SHP.GOOD.TU)) %>%
  mutate(containers = round(IS.SHP.GOOD.TU)) %>%
  select(year, containers)
write.csv(containers.ok, file = "../output/containers.csv", row.names = FALSE)

# Vehicle stock
stock <- read_excel("../input/vehicle-stock.xlsx")
colnames(stock) <- c("year", "conventional_non_oecd", "conventional_oecd")
stock <- mutate(stock, conventional_non_oecd = round(conventional_non_oecd, 1), conventional_oecd = round(conventional_oecd, 1))
write.csv(stock, file = "../output/vehiclestock.csv", row.names = FALSE)

# Emissions by sector
emissector <- read.csv("../input/CO2 emissions by sector-World.csv", skip = 4)
colnames(emissector) <- c("year", "electricity_heat", "other_energy", "industry", "transport", "residential", "services", "agriculture", "fishing", "other_consumption", "units")
emissector.ok <- mutate(emissector, other = agriculture + fishing + other_consumption) %>% 
  select(year, transport, electricity_heat, industry, other_energy, residential, services, other)
write.csv(emissector.ok, file = "../output/sectoremissions.csv", row.names = FALSE)

# Emissions by transport subsector
emistransp <- read.csv("../input/global-co2-emissions-from-transport-by-subsector-2000-2030.csv", skip = 3)
colnames(emistransp) <- c("year", "twothreewheelers", "lightduty", "bus", "rail", "heavytrucks", "shipping", "aviation")

emistransp.ok <- filter(emistransp, !is.na(twothreewheelers), year != "2000", year != "2010") %>%
  separate(year, into = c("year", "type"), sep = " ") %>%
  mutate(type = ifelse(is.na(type), "2020", type)) %>%
  select(year, type, lightduty, heavytrucks, shipping, aviation, bus, twothreewheelers, rail)
write.csv(emistransp.ok, file = "../output/subsectoremissions.csv", row.names = FALSE)

# Per capita transport emissions
gdppcap <- WDI(indicator='NY.GDP.PCAP.PP.CD', extra = T)
gdppcap <- filter(gdppcap, year == 2020) %>%
  select(iso3c, gdppcap = NY.GDP.PCAP.PP.CD)
road.raw <- read_excel("../input/new data.xlsx", sheet = "transport co2 emissions per cap", range="A4:D154")
colnames(road.raw) <- c("iso3c", "country", "transport", "transport_road")
pcap.emistransp.ok <- select(road.raw, iso3c, transport_road) %>%
  mutate(transport_road = as.numeric(gsub(" ", "", transport_road))) %>%
  left_join(gdppcap, by = "iso3c") %>%
  filter(!is.na(gdppcap))
write.csv(pcap.emistransp.ok, file = "../output/transp_emis_pcap.csv", row.names = FALSE)

# Used vehicles
countries.income <- select(countries, iso3c, income_level_iso3c)
used.raw <- read_excel("../input/UNEP Database Traded Vehicles.xlsx", sheet = "Countries by exporter")
used <- select(used.raw, 2, 11:34) %>%
  rename(iso3c = ...2, JPN_2015 = `2015...11`, EU_2015 = `2015...12`, USA_2015 = `2015...13`, KOR_2015 = `2015...14`,
         JPN_2016 = `2016...15`, EU_2016 = `2016...16`, USA_2016 = `2016...17`, KOR_2016 = `2016...18`,
         JPN_2017 = `2017...19`, EU_2017 = `2017...20`, USA_2017 = `2017...21`, KOR_2017 = `2017...22`,
         JPN_2018 = `2018...23`, EU_2018 = `2018...24`, USA_2018 = `2018...25`, KOR_2018 = `2018...26`,
         JPN_2019 = `2019...27`, EU_2019 = `2019...28`, USA_2019 = `2019...29`, KOR_2019 = `2019...30`,
         JPN_2020 = `2020...31`, EU_2020 = `2020...32`, USA_2020 = `2020...33`, KOR_2020 = `2020...34`) %>%
  filter(iso3c != "Country Code") %>%
  pivot_longer(cols = 2:25, names_to = "origin.year", values_to = "vehicles") %>%
  separate(origin.year, into = c("origin", "year"), sep = "_") %>%
  left_join(countries.income, by = "iso3c") %>%
  filter(!is.na(vehicles)) %>%
  mutate(vehicles = as.numeric(vehicles))

by_income_destination <- group_by(used, origin, income_level_iso3c) %>%
  summarise(total = sum(vehicles)) %>%
  filter(income_level_iso3c != "INX", !is.na(income_level_iso3c))

inc.levels <- select(used, iso3c, income_level_iso3c)

used.low <- filter(used, income_level_iso3c %in% c("LIC", "LMC")) %>%
  group_by(iso3c, origin) %>%
  summarise(total = sum(vehicles))

countrytotals <- filter(used, income_level_iso3c %in% c("LIC", "LMC")) %>%
  group_by(iso3c) %>%
  summarise(total = sum(vehicles)) %>%
  mutate(iso3c = if_else(total < 50000, "other", iso3c)) %>%
  group_by(iso3c) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total))
sortedcountries <- countrytotals$iso3c

low.import.countries <- filter(used, income_level_iso3c %in% c("LIC", "LMC")) %>%
  group_by(iso3c) %>%
  summarise(total = sum(vehicles)) %>%
  filter(total < 50000)

used.low.other <- mutate(used.low, iso3c = if_else(iso3c %in% low.import.countries$iso3c, "other", iso3c)) %>%
  group_by(iso3c, origin) %>%
  summarise(total = sum(total))

used.low.other <- mutate(used.low.other, iso3c = factor(iso3c, levels = sortedcountries)) %>%
  arrange(iso3c)

used.low.wide <- pivot_wider(used.low.other, names_from = iso3c, values_from = total)
origins <- c("EU", "JPN", "USA", "KOR")
used.low.wide <- mutate(used.low.wide, origin.fact = factor(origin, levels = origins)) %>%
  arrange(origin.fact) %>%
  select(-origin.fact)

countries <- colnames(used.low.wide)

for(cntr in countries){
  used.low.wide[nrow(used.low.wide) +1,] <- NA
  used.low.wide[nrow(used.low.wide),]$origin <- cntr
}
used.low.wide <- filter(used.low.wide, origin != "origin")
used.low.wide <- used.low.wide[c(5:34,4:1),]

used.low.wide$KOR <- 0
used.low.wide$USA <- 0
used.low.wide$JPN <- 0
used.low.wide$EU <- 0

used.low.wide[is.na(used.low.wide)] <- 0
write.csv(used.low.wide, file = "../output/usedvehicles.csv", row.names = FALSE)

# Used vehicles regulation
regul <- read_excel("../input/map_regulatory_environment_unep.xlsx")
regul.ok <- select(regul, -Country)
colnames(regul.ok) <- c("iso3c", "ranking")
write.csv(regul.ok, file = "../output/usedvehicles_regulation.csv", row.names = FALSE)

# EV sales
evs.income <- read_excel("../input/electric vehicles.xlsx", sheet="ACD", range="C78:O82")
evs.chn <- read_excel("../input/china electric vehicle sales.xlsx") %>%
  filter(`...1` == "Total")
evs.income.chn <- rbind(select(evs.income, -2), evs.chn)

evs.ok <- pivot_longer(evs.income.chn, cols = 2:12, names_to = "year", values_to = "sales") %>%
  rename(income = ...1) %>%
  mutate(income = case_when(
    income == "High income" ~ "HIC",
    income == "Upper middle income" ~ "UMC",
    income == "Lower middle income" ~ "LMC",
    income == "Low income" ~ "LIC",
    income == "Total" ~ "CHN"
  )) %>%
  pivot_wider(names_from = income, values_from = sales)
write.csv(evs.ok, file = "../output/electricvehicles.csv", row.names = FALSE)

# Emissions modes of transport
emmode.raw <- read_excel("../input/CO2 emissions per mode of transport.xlsx", range="B3:D9")
colnames(emmode.raw) <- c("mode", "low", "high")
emmode <- mutate(emmode.raw, mode = recode(mode,
                                           `Walking and biking` = "walkingbiking",
                                           `Bus, bus rapid transit` = "bus",
                                           `Urban rail (metro, tram)` = "rail",
                                           `2- and 3-wheeler` = "wheelers",
                                           `Private car (gasoline or diesel, or hybrid)` = "car",
                                           `Taxi (gasoline, diesel, or hybrid)` = "taxi")) %>%
  arrange(high)
write.csv(emmode, file = "../output/mode_emissions.csv", row.names = FALSE)

# Road traffic emissions
road.raw <- read_excel("../input/new data.xlsx", sheet = "transport ghc emissions by coun")
road <- select(road.raw, countrycode, year, value = data)
road.wide <- pivot_wider(road, names_from = countrycode, values_from = value)
write.csv(road.wide, file = "../output/transport_emissions.csv", row.names = FALSE)

# RAI
rai <- read_excel("../input/rai.xlsx")
rai$iso3c <- countrycode(rai$country, origin = "country.name", destination = "iso3c")
rai <- filter(rai, !(iso3c == "ARE" & year == 2019))
write.csv(rai, file = "../output/rai.csv", row.names = FALSE)

# Air pollution
airdata <- read.csv('../input/data_fig2.csv')
airdata <- filter(airdata, param != "o3")
write.csv(airdata, file = "../output/airpollution.csv", row.names = FALSE)
