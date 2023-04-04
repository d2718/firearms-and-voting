#
# An interactive Shiny app to demonstrate the relationship between
# gun violence and voting habits (and its change) in recent years.
#

library(tidyverse)
library(shiny)

# Natural logarithm of 2; used to calculate log_2.
LOG2 <- log(2.0)
# Location of ticks to label on x-axis of plot.
AXIS_TICKS <- (-4):4
# Labels for ticks on x-axis of plot.
AXIS_TEXT = list(
  "1/16", "1/8", "1/4", "1/2", "1/1", "2/1", "4/1", "8/1", "16/1"
)
# Color to use for linear model line.
MOD_COLOR = rgb(0, 0, 0, alpha=0.5)
# Color to use for aggregate Democratic data.
DEM_COLOR = rgb(0, 0, 1, alpha=0.5)
# Color to use for aggregate Republican data.
GOP_COLOR = rgb(1, 0, 0, alpha=0.5)

# Return the weighted standard deviation of `x`.
wtd.sd <- function(x, weights) {
  normd <- weights / sum(weights)
  x_bar <- weighted.mean(x, normd)
  v <- sum(normd * (x - x_bar)^2)
  sqrt(v)
}

# Return th weighted standard error of the sample mean of `x`.
wtd.se <- function(x, weights) {
  normd <- weights / sum(weights)
  x_bar <- weighted.mean(x, normd)
  v <- sum(normd * (x - x_bar)^2)
  ssq_wgts <- sum(normd^2)
  sqrt(v * ssq_wgts)
}

# Raw data on firearm deaths by county/year.
guns <- read_csv("gun_data.csv") |>
  select(
    year = Year,
    county = COUNTY,
    state = State,
    deaths = Deaths,
    pop = Population
  )

# Raw data on voting by county/year.
pol <- read_csv("voting_data.csv") |>
  select(
    year,
    state = state_po,
    county = county_name,
    party,
    candidate = candidatevotes,
    votes = totalvotes
  )

# Combined data, with voting fractions and log(voting ratio) added,
# as well as some aesthetic values for drawing plots.
dat <- guns |>
  inner_join(pol, join_by(year, state, county)) |>
  pivot_wider(
    names_from = party,
    values_from = candidate
  ) |>
  mutate(
    GREEN = if_else(is.na(GREEN), 0, GREEN),
    OTHER = if_else(is.na(OTHER), 0, OTHER)
  ) |>
  mutate(
    death_rate = 1000000 * deaths / pop,
    dem_frac = DEMOCRAT / votes,
    gop_frac = REPUBLICAN / votes,
    misc_frac = (GREEN + OTHER) / votes,
    loggd = log(REPUBLICAN/DEMOCRAT)/LOG2
  ) |>
  mutate(
    dotcol = rgb(gop_frac, misc_frac, dem_frac, alpha = 0.1),
    dotsize = sqrt(votes) / 250
  )

# Counties/years where the Democratic candidate got the most votes.
dems <- dat |>
  filter(dem_frac > gop_frac & dem_frac > misc_frac)
# Counties/years where the Republican candidate got the most votes.
reps <- dat |>
  filter(gop_frac > dem_frac & gop_frac > misc_frac)

# Years included in the final data.
years <- unique(dat$year)
# Data separated by year.
yearly <- list(dim = length(years))
for (y in 1:length(years)) {
  yr <- years[[y]]
  thisy <- filter(dat, year==yr)
  demy <- filter(dems, year==yr)
  gopy <- filter(reps, year==yr)
  
  obj <- list(
    "dat" = thisy,
    "y" = yr,
    "dem_agg" = 1000000 * sum(demy$deaths) / sum(demy$pop),
    "dem_se"  = wtd.se(demy$death_rate, demy$pop),
    "gop_agg" = 1000000 * sum(gopy$deaths) / sum(gopy$pop),
    "gop_se"  = wtd.se(gopy$death_rate, gopy$pop),
    "ratio_agg" = log(sum(thisy$REPUBLICAN)/sum(thisy$DEMOCRAT))/LOG2,
    "ratio_sd" = wtd.sd(thisy$loggd, thisy$pop)
  )
  yearly[[y]] <- obj
}

ui <- fluidPage(
  includeCSS("firearms.css"),
  verticalLayout(
    titlePanel("Voting Preference and Firearm Deaths"),

    sidebarLayout(
      position = "right",
      sidebarPanel(
        radioButtons(
          "year", h3("Select Year"),
          choices = list(
            "2000" = 1,
            "2004" = 2,
            "2008" = 3,
            "2012" = 4,
            "2016" = 5
          ),
          selected = 1
        )
      ),
      
      mainPanel(
        verticalLayout(
          htmlOutput("year"),
          plotOutput("the_plot"),
          htmlOutput("extra"),
          withMathJax(
            includeMarkdown("supp.md")
          )
        )
      )
      
    )
  )
)

server <- function(input, output) {
  output$year <- renderUI({
    n <- as.integer(input$year)
    h3(years[[n]], style = "font-size: 20pt; text-align: center; margin-bottom: 0;")
  })
  
  yearly_data <- reactive({
    n <- as.integer(input$year)
    yearly[[n]]
  })
  
  output$the_plot <- renderPlot({
    yearly_dat <- yearly_data();
    d <- yearly_dat$dat
    plot(
      d$loggd, d$death_rate,
      ylab="Deaths per One Million Population",
      xlab="GOP votes / DEM votes (log scale)",
      col=d$dotcol, pch=19, cex=d$dotsize,
      xlim=c(-4, 4), xaxt="n",
      ylim=c(0, 500)
    )
    abline(lm(d$death_rate ~ d$loggd), col=MOD_COLOR, lwd=3)
    axis(1, at=AXIS_TICKS, lab=AXIS_TEXT)

    conf <- c(-1.96, 1.96)
    dem_rng <- yearly_dat$dem_agg + (conf * yearly_dat$dem_se)
    gop_rng <- yearly_dat$gop_agg + (conf * yearly_dat$gop_se)
    ratio_rng <- yearly_dat$ratio_agg + (c(-1, 1) * yearly_dat$ratio_sd)
    abline(h = yearly_dat$dem_agg, col=DEM_COLOR, lwd=3)
    abline(h = dem_rng, col=DEM_COLOR)
    abline(h = yearly_dat$gop_agg, col=GOP_COLOR, lwd=3)
    abline(h = gop_rng, col=GOP_COLOR)
    abline(v = ratio_rng, col=MOD_COLOR)
    legend("topleft",
      legend = c("Linear Fit", "DEM avg", "GOP avg"),
      col = c(MOD_COLOR, DEM_COLOR, GOP_COLOR),
      lwd=3, bty="n"
    )
  })

  output$extra <- renderUI({
    yearly_dat <- yearly_data();
    d <- yearly_dat$dat
    p(
      paste(
        length(d$county),
        "counties from",
        length(unique(d$state)),
        "states."
      ),
      style = "text-align: right;"
    )
  })
}

shinyApp(ui = ui, server = server)