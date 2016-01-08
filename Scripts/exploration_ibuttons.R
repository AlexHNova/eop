# Date created: 17 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare...
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

df_ibutton_data_classified$type <- paste(df_ibutton_data_classified$energy_type, df_ibutton_data_classified$insulation_type, sep = "_")

monper1 <- df_ibutton_data_classified[which(df_ibutton_data_classified$mon_per == 1),] 
monper2 <- df_ibutton_data_classified[which(df_ibutton_data_classified$mon_per == 2),]
monper3 <- df_ibutton_data_classified[which(df_ibutton_data_classified$mon_per == 3),]

ggplot(data = df_ibutton_data_classified, mapping = aes(x = date, y = w, group = type, colour = type)) + 
  geom_smooth() + 
  ggtitle("Avg indoor temperature per intervention type") + 
  labs(x = "Date (mid-June to mid-October)", y = "Avg indoor temperature (degr. Celsius)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ggplot(data = df_ibutton_data_classified, mapping = aes(x = date, y = w)) + 
#   geom_smooth() + 
#   ggtitle("Avg indoor temperature per intervention type: separated") + 
#   labs(x = "Date (mid-June to mid-October)", y = "Avg indoor temperature (degr. Celsius)") +
#   facet_grid(facets = energy_type ~ insulation_type) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

ib_m <- melt(data = df_ibutton_data_classified[,c("date", "stand_number", "type", "w", "c", "fire")], id.vars = c("date", "stand_number", "type"), factorsAsStrings = TRUE, na.rm = TRUE)

ib_m_nofire <- ib_m[which(ib_m$variable %in% c("c", "w")),]
ib_m_nofire$value <- as.numeric(ib_m_nofire$value)


ggplot(data = ib_m_nofire, mapping = aes(x = date, y = value, colour = variable)) + 
  geom_smooth() + 
  facet_wrap(facets = ~ type, nrow = 4, ncol = 2) +
  ggtitle("Avg stove temperatures (c) vs. avg indoor temperatures (w); grouped by intervention type") +
  labs(x = "Date (mid-June to mid-October)", y = "temp (degr. Celsius)")

ib_m_nofire$hod <- hour(x = ib_m_nofire$date)

# ggplot(data = ib_m_nofire, mapping = aes(x = hod, y = value, colour = variable)) + 
#   geom_smooth() + 
#   facet_wrap(facets = ~ type, nrow = 4, ncol = 2) +
#   ggtitle("Avg stove temperatures (c) vs. avg indoor temperatures (w); by hour of day") +
#   labs(x = "Hour of Day (00 - 23)", y = "temp (degr. Celsius)")

df_ibutton_data_classified$hod <- hour(df_ibutton_data_classified$date)

ggplot(data = df_ibutton_data_classified, mapping = aes(x = hod, y = c, group = type, colour = type)) + 
  geom_smooth() + 
  ggtitle("Avg stove temperature per intervention type; by hour of day") + 
  labs(x = "Hour of Day (00 - 23)", y = "Avg stove temperature (degr. Celsius)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = df_ibutton_data_classified, mapping = aes(x = hod, y = w, group = type, colour = type)) + 
  geom_smooth() + 
  ggtitle("Avg indoor temperature per intervention type; by hour of day") + 
  labs(x = "Hour of Day (00 - 23)", y = "Avg indoor temperature (degr. Celsius)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
