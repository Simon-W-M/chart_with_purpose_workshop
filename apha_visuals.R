library(tidyverse)
library(CGPfunctions)
library(datasauRus)
library(gt)
library(gtExtras)
library(NHSRdatasets)
library(ggbump)
library(gghighlight)
library(geomtextpath)
library(scales)
library(data.table)
library(ggExtra)
library(ggtext)
library(gtsummary)
library(ggpubr)

# very messy scrap works to build charts for 'Charting with Purpose'
# workshop - some the visuals didn't make it to the final
# workshop but I have kept the code here anyways in case
# it comes in handy


# first data
dat <- data.frame(
  team = c("Bulbasaur Ward", 
           "Pikachu Ward", 
           "Charmander Ward", 
           "Squirtle Ward", 
           "Ash Katchum Ward"),
  percent = c(19, 22, 21, 18, 20)
)

# pie chart
dat |>
  ggplot(aes(x = "", y = percent, fill = team)) +
  geom_col(color = "black") +
  # geom_text(aes(label = value),
  #          position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void() +
  labs(title = "Percentage occupancy by ward")

# bar chart
dat |>
  ggplot(aes(y = percent, x = team, fill = team)) +
  geom_col() +
  theme_minimal() +
  coord_cartesian(ylim = c(17, 23)) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Percentage occupancy by ward")



# ordered bar chart
dat |>
  ggplot(aes(x = percent, y = reorder(team, percent), fill = -percent)) +
  geom_col() +
  theme_void() +
  geom_text(aes(x = percent - 2, y = reorder(team, percent), label = paste0(percent, "%")),
    colour = "white", size = 6
  ) +
  geom_text(aes(x = 1, y = reorder(team, percent), label = team),
    colour = "white", size = 6, hjust = "left"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


# refresh data
dat <- data.frame(
  team = c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"),
  percent = c(24, 19, 29, 14, 18)
)

# add in conditional colour
dat <- dat |>
  mutate(col = if_else(percent > 20, "a", "b"))

# create list of those over 20% for labels
over20 <- toString(dat$team[dat$percent > 20])
over20 <- sub(",([^,]*)$", " and\\1", over20)

# choose colours for charts
rhg_cols <- c("blue", "grey")

# plot with hightlights and useful title
dat |>
  ggplot(aes(
    x = percent,
    y = reorder(
      team,
      percent
    ),
    fill = factor(col)
  )) +
  geom_col() +
  theme_void() +
  geom_text(
    aes(
      x = percent - 2,
      y = reorder(
        team,
        percent
      ),
      label = paste0(percent, "%")
    ),
    colour = "white",
    size = 6
  ) +
  geom_text(
    aes(
      x = 1,
      y = reorder(team, percent),
      label = team
    ),
    colour = "white",
    size = 6,
    hjust = "left"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  scale_fill_manual(values = rhg_cols) +
  labs(
    title = paste0(over20, " are showing over 20% occupancy"),
    subtitle = paste0(
      "Chart showing bed occupancy percentage as at ",
      format(Sys.Date(), "%d %b %y")
    ),
    caption = paste0(
      "Data downloaded: ",
      format(Sys.Date(), "%d/%m/%y")
    )
  )


dat2 <- data.frame(
  team = c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"),
  percent = c(20, 24, 25, 17, 16)
)

# second chart to do comparison
dat2 |>
  ggplot(aes(x = percent, y = reorder(team, percent), fill = -percent)) +
  geom_col() +
  theme_void() +
  geom_text(aes(x = percent - 2, y = reorder(team, percent), label = paste0(percent, "%")),
    colour = "white", size = 6
  ) +
  geom_text(aes(x = 1, y = reorder(team, percent), label = team),
    colour = "white", size = 6, hjust = "left"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  labs(title = "KPI position as at 2024")

dat <- data.frame(
  team = c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"),
  percent = c(19, 22, 21, 18, 19)
)

# add years to data
dat$year <- "2024"
dat2$year <- "2025"

# merge the data into one dataframe
dat3 <- bind_rows(dat, dat2)

# stacked bar chart
dat3 |>
  ggplot(aes(fill = team, y = percent, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal()

# multiple column chart
dat3 |>
  ggplot(aes(y = percent, x = team)) +
  geom_col(aes(fill = year, group = year), position = "dodge", colour = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# turn years into factors
dat3$year <- as.factor(dat3$year)

dat3 <- dat3 |>
  mutate(col = if_else(team == "Charmander Ward", "Blue", "Grey"))

cols <- c("Grey", "Grey", "Blue", "Grey", "Grey")

# slope graph
newggslopegraph(
  dataframe = dat3,
  Title = "Charmader Ward has seen the largest increase 2024-25",
  SubTitle = "Percentage change by team 2024-25",
  Caption = "Based on: Edward Tufte, Beautiful Evidence (2006), pages 174-176.",
  Times = year,
  Measurement = percent,
  Grouping = team,
  LineColor = cols,
  YTextSize = 4
)


dat4 <- data.frame(
  team = c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"),
  percent = c(12, 18, 24, 23, 21),
  year = c(rep("2026", 5))
)


dat$year <- "2024"
dat2$year <- "2025"

dat5 <- bind_rows(dat3, dat4)

dat5$year <- as.numeric(dat5$year)

# bump chart - ended not not using this
ggplot(dat5, aes(x = year, y = percent, color = team)) +
  geom_bump(size = 1.5) +
  geom_point(size = 6) +
  geom_text(
    data = dat5 %>% filter(year == min(year)),
    aes(x = year - 0.1, label = team),
    size = 5, hjust = 1
  ) +
  geom_text(
    data = dat5 %>% filter(year == max(year)),
    aes(x = year + 0.1, label = team),
    size = 5, hjust = 0
  ) +
  # scale_color_brewer(palette = "RdBu") +
  # theme_void() +
  theme(legend.position = "none")

## data saurus

# datasaurus table
datasaurus_dozen |>
  group_by(dataset) |>
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  ) |>
  gt()

# datasaurus plots
ggplot(datasaurus_dozen |> filter(!dataset == "circle"), aes(x = x, y = y, colour = dataset)) +
  geom_point() +
  theme_void() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~dataset, ncol = 3)

# spaghetti data
dates <- rep(seq(as.Date("2024/1/1"), by = "month", length.out = 18), 5)
teams <- sort(rep(c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"), 18))
norm1 <- round(rnorm(18, 20000, 500), 0)
norm2 <- round(rnorm(18, 22000, 2500), 0)
norm3 <- round(rnorm(18, 22000, 1000), 0)
norm4 <- round(rnorm(18, 23000, 500), 0)
norm5 <- round(rnorm(9, 20000, 1500), 0)
norm6 <- round(rnorm(9, 23000, 1000), 0)
attendances <- c(norm1, norm2, norm3, norm4, norm5, norm6)

data <- data.frame(teams, dates, attendances)

zdata <- data

# spagetti chart
data |> ggplot(aes(
  x = dates,
  y = attendances,
  colour = teams
)) +
  geom_line(size = 1) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "2 month"
  )

# small multiples facet chart
data |> ggplot(aes(
  x = dates,
  y = attendances
)) +
  geom_line(size = 1) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "3 month"
  ) +
  facet_grid(~teams)

data |> ggplot(aes(
  x = dates,
  y = attendances,
  colour = teams
)) +
  geom_line(size = 1) +
  scale_x_date(
    date_labels = "%b, %y",
    date_breaks = "3 month"
  ) +
  gghighlight(teams == "Pikachu Ward",
    line_label_type = "sec_axis"
  ) +
  theme_minimal()

latest_full_mth <- max(dates)
intervention_date <- as.Date("2024-10-01")
y_min <- min(data$attendances[data$teams == "Squirtle Ward" & data$dates >= intervention_date])
y_max <- max(data$attendances[data$teams == "Squirtle Ward" & data$dates >= intervention_date])

start <- min(dates)
int_dat_less_one <- intervention_date - months(1)
y_min_pre <- min(data$attendances[data$teams == "Squirtle Ward" & data$dates <= int_dat_less_one])
y_max_pre <- max(data$attendances[data$teams == "Squirtle Ward" & data$dates <= int_dat_less_one])

# full chart with annotations
data |> ggplot(aes(
  x = dates,
  y = attendances,
  colour = teams
)) +
  geom_line(size = 1) +
  scale_x_date(
    date_labels = "%b, %y",
    date_breaks = "2 month"
  ) +
  gghighlight(teams == "Squirtle Ward",
    line_label_type = "sec_axis",
    label_params = list(size = 3)
  ) +
  theme_minimal() +
  annotate("rect",
    xmin = intervention_date, xmax = latest_full_mth,
    ymin = y_min, ymax = y_max,
    fill = "lightblue", alpha = 0.5
  ) +
  annotate("rect",
    xmin = start, xmax = int_dat_less_one,
    ymin = y_min_pre, ymax = y_max_pre,
    fill = "lightgrey", alpha = 0.5
  ) +
  geom_vline(
    xintercept = intervention_date,
    color = "blue", linetype = "dashed", size = 1
  ) +
  geom_text(
    aes(
      x = intervention_date - months(3),
      y = 15000, label = "Start of intervention"
    ),
    colour = "blue", size = 3.5
  ) +
  geom_text(
    aes(
      x = intervention_date + months(2),
      y = 24500, label = "Improvement"
    ),
    colour = "blue", size = 3.5, alpha = 0.7
  ) +
  geom_text(
    aes(
      x = intervention_date - months(7),
      y = 22500, label = "Pre intervention"
    ),
    colour = "black", size = 3.5, alpha = 0.7
  ) +
  labs(
    title = "Squirtle Ward has shown improvement since intervention",
    subtitle = "Chart showing attendances Jan 24 - Jun 25 by ward",
    caption = "Data download from UDAL as at 27/08/25"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# real ae figures

ae <- data.frame(
  stringsAsFactors = FALSE,
  Month = c("2025-01-01", "2025-02-01", "2025-03-01"),
  AE_Attendances = c(2218130, 2088071, 2389064)
)

ae |>
  gt() |>
  fmt_date(
    columns = Month,
    date_style = "yMMM"
  ) |>
  fmt_number(
    columns = AE_Attendances,
    decimals = 0,
    sep_mark = ","
  )

ae <- ae |>
  mutate(
    Month_d = as.Date(Month),
    Days_in_month = days_in_month(Month_d),
    `Rate_attendances (Attendances / Days in Month)` = AE_Attendances / Days_in_month
  ) |>
  select(-Month_d)

ae |>
  gt() |>
  fmt_date(
    columns = Month,
    date_style = "yMMM"
  ) |>
  fmt_number(
    columns = AE_Attendances,
    decimals = 0,
    sep_mark = ","
  ) |>
  fmt_number(
    columns = `Rate_attendances (Attendances / Days in Month)`,
    decimals = 1,
    sep_mark = ","
  )


# potentially spurious correlations
orange <- "#FFA500"
lightblue <- "#0391BF"
darkblue <- "#003087"

dates <- seq(as.Date("2024/1/1"), 
             by = "month", 
             length.out = 18)
attendances <- round(rnorm(18, 2000, 100), 0)
percent_admissions <- round(rnorm(18, 10, 2), 1)

data <- data.frame(dates, 
                   attendances, 
                   percent_admissions)

## Alternate with ggtext
ggplot(data, aes(dates, attendances)) +
  geom_col(fill = lightblue) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggExtra::removeGridX() +
  labs(
    x = "",
    y = "Count of attendances",
    caption = "Data downloaded from UDAL 28-08-25",
    title = "<b><span style = 'color:#999999;'>18 month </span><span style = 'color:#0391BF;'>Referrals</span><span style = 'color:#999999;'> and</span> <span style = 'color:#000000;'>Admission Percentages.</span></b>"
  ) +
  theme(
    plot.title = element_textbox_simple(
      size = 14,
      lineheight = 1,
      padding = margin(0, 0, 5, 0)
    ),
    legend.position = "top",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0),
    axis.title.y.right = element_text(angle = 90)
  ) +
  geom_line(data,
    mapping = aes(dates, percent_admissions * 200),
    colour = "black", linewidth = 1.2, alpha = 0.5
  ) +
  geom_point(data,
    mapping = aes(dates, percent_admissions * 200),
    colour = "black", size = 2, alpha = 0.5
  ) +
  scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / 19000,
    name = "Percentage of admissions",
    labels = scales::percent
  ))

mod <- lm(attendances ~ percent_admissions, data = data)

mod |> tbl_regression(intercept = TRUE)

datac <- data

ggscatter(data,
  x = "percent_admissions", y = "attendances",
  color = "black", size = 1, # Points color, shape and size
  add = "reg.line", # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
  conf.int = TRUE, # Add confidence interval
  cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
  title = "Correlation betweeen admissions and attendances"
)

# trendline

data |> ggplot() +
  aes(x = dates, y = attendances) +
  geom_line() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  scale_x_date(
    date_labels = "%b %y ",
    date_breaks = "2 month"
  ) +
  labs(title = "Number of attednaces is showing a downward trend")

mod <- lm(attendances ~ dates, data = data)

mod |> tbl_regression(intercept = TRUE)


# spaghetti data
dates <- rep(rep(seq(as.Date("2024/6/1"), by = "month", length.out = 3), 5), 60)
teams <- sort(rep(c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"), 180))
norm1 <- round(rnorm(180, 20, 5), 0)
norm2 <- round(rnorm(180, 25, 5), 0)
norm3 <- round(rexp(180, .1), 0)
norm4 <- round(rexp(180, .1), 0)
norm5 <- round(rexp(180, .08), 0)
los <- c(norm1, norm2, norm3, norm4, norm5)

data <- data.frame(teams, dates, los)

data <- data |>
  mutate(
    mn = mean(los),
    med = median(los),
    .by = c(dates, teams)
  )

data |>
  filter(teams == "Squirtle Ward") |>
  ggplot() +
  aes(x = los) +
  geom_density() +
  facet_grid(~dates)

data |>
  filter(teams == "Squirtle Ward") |>
  ggplot() +
  aes(x = los, y = dates, color = dates) +
  geom_jitter()

data |>
  filter(teams == "Squirtle Ward") |>
  ggplot() +
  aes(x = los, y = dates, color = dates) +
  geom_jitter() +
  geom_point(aes(x = mn, y = dates), colour = "red", size = 5) +
  geom_point(aes(x = med, y = dates), colour = "green", size = 5)


data |>
  filter(teams == "Pikachu Ward") |>
  ggplot() +
  aes(x = los, y = dates, color = dates) +
  geom_jitter() +
  geom_point(aes(x = mn, y = dates), colour = "red", size = 5) +
  geom_point(aes(x = med, y = dates), colour = "green", size = 5)

data |>
  filter(teams == "Bulbasaur Ward") |>
  ggplot() +
  aes(x = los, y = dates, color = dates) +
  geom_jitter() +
  geom_point(aes(x = mn, y = dates), colour = "#DA291C", size = 5, alpha = 0.1) +
  geom_point(aes(x = med, y = dates, alpha = 0.1), colour = "#009639", size = 5, alpha = 0.1) +
  theme(legend.position = "none") +
  labs(
    title = "3 Months of data - normally distributed",
    subtitle = "<span style = 'color:#999999;'>Chart Showing</span> <span style = 'color:#DA291C;'>Mean</span><span style = 'color:#999999;'> and </span><span style = 'color:#009639;'>Median</span>"
  ) +
  theme(plot.subtitle = element_markdown())

dat_p <- data |>
  filter(
    teams == "Bulbasaur Ward",
    dates == "2024-06-01"
  )

gghistogram(dat_p, x = "los", rug = TRUE, fill = "lightgrey", add_density = TRUE) +
  geom_textvline(
    xintercept = dat_p$mn,
    colour = "red",
    label = "Mean",
    vjust = -0.2,
    hjust = 0.9
  ) +
  geom_textvline(
    xintercept = dat_p$med,
    colour = "blue",
    angle = 90,
    label = "Median",
    vjust = -0.2,
    hjust = 0.1
  )

dat_p <- data |>
  filter(
    teams == "Pikachu Ward",
    dates == "2024-06-01"
  )

gghistogram(dat_p, x = "los", rug = TRUE, fill = "lightgrey", add_density = TRUE) +
  geom_textvline(
    xintercept = dat_p$mn,
    colour = "red",
    label = "Mean",
    vjust = -0.2,
    hjust = 0.9
  ) +
  geom_textvline(
    xintercept = dat_p$med,
    colour = "blue",
    angle = 90,
    label = "Median",
    vjust = -0.2,
    hjust = 0.1
  )


dat_p <- data |>
  filter(
    teams == "Squirtle Ward",
    dates == "2024-06-01"
  )

gghistogram(dat_p, x = "los", rug = TRUE, fill = "lightgrey", add_density = TRUE) +
  geom_textvline(
    xintercept = dat_p$mn,
    colour = "red",
    label = "Mean",
    vjust = -0.2,
    hjust = 0.9
  ) +
  geom_textvline(
    xintercept = dat_p$med,
    colour = "blue",
    angle = 90,
    label = "Median",
    vjust = -0.2,
    hjust = 0.1
  )


# sparky data
teams <- sort(rep(c("Bulbasaur Ward", "Pikachu Ward", "Charmander Ward", "Squirtle Ward", "Ash Katchum Ward"), 18))
norm1 <- round(rnorm(18, 200, 70), 0)
norm2 <- round(rnorm(18, 25, 10), 0)
norm3 <- round(rnorm(18, 250, 100), 0)
norm4 <- c(30.24, 30.21, 30.24, 30.26, 30.19, 30.23, 30.25, 30.28, 30.27, 30.30, 30.32, 30.29, 30.31, 30.32, 30.33, 30.29, 30.30, 30.31)
norm5 <- round(rnorm(18, 25, 15), 0)
kpi <- c(norm1, norm2, norm3, norm4, norm5)
kpi_val <- c(
  rep(tail(norm1, 1), 18), rep(tail(norm2, 1), 18),
  rep(tail(norm3, 1), 18), round(rep(tail(norm4, 1), 18), 0), #
  rep(tail(norm5, 1), 18)
)
period <- rep(seq(1:18), 5)

s_data <- data.frame(teams, kpi_val, kpi, period)



s_data |>
  summarise(
    kpi_val = max(kpi_val),
    spark = list(kpi),
    .by = c(teams)
  ) |>
  gt() |>
  gt_plt_sparkline(spark,
    label = FALSE,
    same_limit = FALSE
  )

s_data <- s_data |>
  mutate(
    start = if_else(period == 1, kpi, NA),
    end = if_else(period == 18, kpi, NA)
  )

library(ggspark)
library(ggrepel)
ggplot(s_data, aes(period, kpi, group = teams)) +
  geom_line(colour = "grey") +
  stat_sparklabels(
    geom = "point", label_fun = \(x) round(x, 1),
    show.legend = FALSE
  ) +
  stat_sparklabels(
    geom = "text_repel", label_fun = \(x) round(x, 1),
    show.legend = FALSE
  ) +
  scale_colour_manual("", values = c("black", "blue", "red")) +
  # scale_y_continuous(limits = c(0, 25)) +
  facet_grid(teams ~ .,
    scales = "free"
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line()
  )


shark_attacks <- rnorm(20, 200, 20)
ice_cream_sales <- rnorm(20, 20, 5)
shk <- data.frame(
  shark_attacks,
  ice_cream_sales
)

shk <- shk |>
  mutate(ice_cream_sales = shark_attacks + ice_cream_sales)

model_plot <- function(data) {
  dat_mod <- data

  # x_max_lim <- max(dat_mod$wk_mins_delay)
  # x_max_lim <- plyr::round_any(x_max_lim,500, f = ceiling)

  mod <- lm(ice_cream_sales ~ shark_attacks, data = dat_mod)
  mod_sum <- summary(mod)

  ggplot(dat_mod, aes(x = shark_attacks, y = ice_cream_sales)) +
    geom_point() +
    # ylim(0,2) +
    # xlim(0,x_max_lim)+
    geom_smooth(method = "lm", fullrange = TRUE, level = 0) +
    theme_minimal() +
    xlab("Shark attacks") +
    ylab("Ice cream sales") +
    labs(
      subtitle = paste(
        "Adj R2 = ", signif(summary(mod)$adj.r.squared, 5),
        "Intercept =", signif(mod$coef[[1]], 5),
        " Slope =", signif(mod$coef[[2]], 5),
        " P =<", plyr::round_any(signif(summary(mod)$coef[2, 4], 5), 0.001, f = ceiling)
      ),
      title = paste0("Shark attacks and Ice Cream sales are highly correlated")
    )
}

model_plot(shk)
