USAid <- USAid %>%
  mutate(Agency = if_else(is.na(Agency) | Agency == "", "missing", Agency))
table(USAid$Agency)



ggplot(USAid, aes(x = year, y = Amount, color = Agency)) +
  geom_line() +
  geom_point()

library(ggplot2)
library(dplyr)

# Convert year to Date format
USAID_agency_all <- USAID_agency_all %>%
  mutate(year = as.Date(paste0(year, "-01-01")))

last_year_data <- USAID_agency_all %>%
  group_by(Agency) %>%
  filter(year == max(year)) %>%
  ungroup()

important_years <- data.frame(
  year = as.Date(c(
    "2001-10-07",  # Start of U.S. War in Afghanistan
    "2003-03-20",  # Start of U.S. War in Iraq
    "2004-01-20",  # Bush second term begins
    "2009-01-20",  # Obama assumes presidency
    "2009-12-01",  # Obama announces troop surge
    "2014-12-28",  # End of ISAF mission
    "2017-01-20",  # Trump assumes presidency
    "2017-08-21",  # Trump announces Afghanistan strategy
    "2020-02-29",  # U.S.-Taliban Agreement signed
    "2021-01-20",  # Biden assumes presidency
    "2021-08-15"  # Taliban Takeover of Kabul
  )),
  label = c(
    "Start of U.S. War in Afghanistan",
    "Start of U.S. War in Iraq",
    "Bush Second Term Begins",
    "Obama Assumes Presidency",
    "Obama Announces Troop Surge",
    "End of ISAF Mission",
    "Trump Assumes Presidency",
    "Trump Announces Afghanistan Strategy",
    "U.S.-Taliban Agreement",
    "Biden Assumes Presidency",
    "Taliban Takeover of Kabul"
  )
)


# Creating the plot
ggplot(USAID_agency_all, aes(x = year, y = TotalAid, color = Agency, group = Agency)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_label(data = last_year_data,
             aes(x = year, y = TotalAid, label = Agency),
             size = 4, fill = "white", label.size = NA, nudge_x = 10, show.legend = FALSE) +
  geom_vline(data = important_years, aes(xintercept = year), linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = important_years$year,
           y = max(USAID_agency_all$TotalAid, na.rm = TRUE) * 0.9,
           label = important_years$label,
           color = "red", angle = 90, vjust = -0.5, size = 4) +
  scale_y_continuous(name = "Total Aid in Million (USD)") +
  ggtitle("USA Aid to Afghanistan By Agency: 2002-2024") +
  labs(caption = "© Qayoom Suroush, 2025. Data Source: SIGAR.") +  
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(size = 0.3),
    panel.grid.minor = element_line(size = 0.15),
    plot.margin = margin(t = 14, r = 40, b = 3, l = 1.5),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic") 
  )

