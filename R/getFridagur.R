
getEaster <- function(Y = NULL) {
  # Input integer value representing year. Returns date of
  # easter (as 'Date' type).
  #
  # Check formating:
  if (!is.numeric(Y)) {
    if (is.null(Y))
      Y <- Sys.Date()
    if (inherits(Y, "Date"))
      Y <- format(Y, "%Y")
    Y <- as.numeric(Y)
  }
  #
  # The Meeus/Jones/Butcher algorithm
  a <- Y %% 19
  b <- floor(Y / 100)
  c <- Y %% 100
  d <- floor(b / 4)
  e <- b %% 4
  f <- floor((b + 8) / 25)
  g <- floor((b - f + 1) / 3)
  h <- (19 * a + b - d - g + 15) %% 30
  i <- floor(c / 4)
  k <- c %% 4
  L <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- floor((a + 11 * h + 22 * L) / 451)
  month <- floor((h + L - 7 * m + 114) / 31)
  day <- ((h + L - 7 * m + 114) %% 31) + 1
  as.Date(paste(Y, month, day, sep="-"))
}

getSumardagur <- function(y = NULL) {
  # Input integer value representing year. Returns date of
  # fyrsti sumardagurinn (as 'Date' type).
  #
  # Check formating:
  if (!is.numeric(y)) {
    if (is.null(y))
      y <- Sys.Date()
    if (inherits(y, "Date"))
      y <- format(y, "%Y")
    y <- as.numeric(y)
  }
  #
  # Fyrsti fimmtudagurinn eftir 18. apríl:
  if (length(y) > 1) {
    do.call("c", lapply(y, getSumardagur))
  } else {
    x <- seq(
      as.Date(paste(y, "04/19", sep = "/")),
      as.Date(paste(y, "04/25", sep = "/")),
      by = "1 day"
    )
    x[which(as.POSIXlt(x)$wday == 4)]
  }
}

getVerslunardag <- function(y = NULL) {
  # Input integer value representing year. Returns date of
  # frídagur verzlunarmanna (as 'Date' type).
  #
  # Check formating:
  if (!is.numeric(y)) {
    if (is.null(y))
      y <- Sys.Date()
    if (inherits(y, "Date"))
      y <- format(y, "%Y")
    y <- as.numeric(y)
  }
  #
  # Fyrsti mánudagurinn í ágúst.
  if (length(y) > 1) {
    do.call("c", lapply(y, getVerslunardag))
  } else {
    x <- seq(
      as.Date(paste(y, "08/01", sep = "/")),
      as.Date(paste(y, "08/07", sep = "/")),
      by = "1 day"
    )
    x[which(as.POSIXlt(x)$wday == 1)]
  }
}

getSjomannadag <- function(y = NULL) {
  # Input integer value representing year. Returns date of
  # Sjómannadagur (as 'Date' type).
  #
  # Check formating:
  if (!is.numeric(y)) {
    if (is.null(y))
      y <- Sys.Date()
    if (inherits(y, "Date"))
      y <- format(y, "%Y")
    y <- as.numeric(y)
  }
  #
  # Fyrsti sunnudagurinn í júní, nema ef það er hvítasunnudagur (þá +7).
  if (length(y) > 1) {
    do.call("c", lapply(y, getSjomannadag))
  } else {
    # Check exeptions:
    if (y == 1938 | y == 1963) {
      x <- getEaster(y) + 50      # Haldið annan í hvítasunnu
    } else if (y <= 1968 & y >= 1965) {
      # Veit ekki hvað var í gangi hérna ... en þetta voru dagarnir:
      if (y == 1965) x <- as.Date("1965/05/30")
      if (y == 1966) x <- as.Date("1966/05/15")
      if (y == 1967) x <- as.Date("1967/05/28")
      if (y == 1968) x <- as.Date("1968/05/26")
    } else if (y == 1986) {
      x <- as.Date("1986/06/08")  # Frestað vegna sveitasjórnarkosninga
    } else {
      # This is the general rule by law 20/1987 "Lög um Sjómannadag"
      x <- seq(
        as.Date(paste(y, "06/01", sep = "/")),
        as.Date(paste(y, "06/07", sep = "/")),
        by = "1 day"
      )
      x <- x[which(as.POSIXlt(x)$wday == 0)]
      if (x == getEaster(y) + 49)
        x <- x + 7
    }
    x
  }
}

getFridagur <- function(x = Sys.Date(), days = c("all", "whole", "half", "none"), weekend = TRUE, name = FALSE, abbreviate = FALSE) {
  # Input date (as 'Date' type). Returns indicator variable of whether it is
  # an Icelandic public holiday (as 'logic' type, i.e. TRUE/FALSE). If the
  # input parameter 'name' is TRUE the public holiday names are return.
  # Example:
  #   getFridagur()
  #   getFridagur(seq(as.Date("2011/1/1"), as.Date("2013/12/31"), by="1 day"))
  #   getFridagur(seq(as.Date("2011/1/1"), as.Date("2013/12/31"), by="1 day"), weekend=TRUE, name=TRUE)
  #   getFridagur(seq(as.Date("2012/1/1"), as.Date("2012/12/31"), by="1 day"), name=TRUE)
  #   data.frame(
  #      Dags = seq(as.Date("2013/1/1"), as.Date("2013/12/31"), by="1 day"),
  #   	 Dagur = getFridagur(seq(as.Date("2013/1/1"), as.Date("2013/12/31"), by="1 day"), weekend=TRUE, name=TRUE))
  #
  days <- match.arg(days)
  #
  xx <- format(x, "%d.%m")         # Day & month for calendar date specific days.
  y <- as.integer(format(x, "%Y")) # Exctract year to find moveable days.
  y_unique <- unique(y)            # We only need to call the functions ones for each year.
  rr <- match(y, y_unique)         # Row indexing the years.
  #
  paskar <- getEaster(y_unique)[rr]
  sumardagur <- getSumardagur(y_unique)[rr]
  versldagur <- getVerslunardag(y_unique)[rr]
  #
  if (name) {
    ans <- rep("", NROW(x))
    if (weekend) {
      rr <- which(as.POSIXlt(x)$wday == 6 | as.POSIXlt(x)$wday == 0)
      if (length(rr) > 0) ans[rr] <- weekdays(x[rr], abbreviate=abbreviate)
    }
    if (days == "all" || days == "whole") {
      rr <- which(xx == "01.01");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Nýársdagur", paste(ans[rr], "Nýársdagur", sep = ", "))
      rr <- which(x == paskar - 3);  if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Skírdagur", paste(ans[rr], "Skírdagur", sep = ", "))
      rr <- which(x == paskar - 2);  if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Föstudagurinn langi", paste(ans[rr], "Föstudagurinn langi", sep = ", "))
      rr <- which(x == paskar);      if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Páskadagur", paste(ans[rr], "Páskadagur", sep = ", "))
      rr <- which(x == paskar + 1);  if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Annar í páskum", paste(ans[rr], "Annar í páskum", sep = ", "))
      rr <- which(x == sumardagur);  if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Sumardagurinn fyrsti", paste(ans[rr], "Sumardagurinn fyrsti", sep = ", "))
      rr <- which(xx == "01.05");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Verkalýðsdagurinn", paste(ans[rr], "Verkalýðsdagurinn", sep = ", "))
      rr <- which(x == paskar + 39); if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Uppstigningardagur", paste(ans[rr], "Uppstigningardagur", sep = ", "))
      rr <- which(x == paskar + 49); if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Hvítasunnudagur", paste(ans[rr], "Hvítasunnudagur", sep = ", "))
      rr <- which(x == paskar + 50); if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Annar í hvítasunnu", paste(ans[rr], "Annar í hvítasunnu", sep = ", "))
      rr <- which(xx == "17.06");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Þjóðhátíðardagurinn", paste(ans[rr], "Þjóðhátíðardagurinn", sep = ", "))
      rr <- which(x == versldagur);  if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Frídagur verslunarmanna", paste(ans[rr], "Frídagur verslunarmanna", sep = ", "))
      rr <- which(xx == "25.12");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Jóladagur", paste(ans[rr], "Jóladagur", sep = ", "))
      rr <- which(xx == "26.12");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Annar í jólum", paste(ans[rr], "Annar í jólum", sep = ", "))
    }
    if (days == "all" || days == "half")
    {
      rr <- which(xx == "24.12");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Aðfangadagur", paste(ans[rr], "Aðfangadagur", sep = ", "))
      rr <- which(xx == "31.12");    if (length(rr) > 0) ans[rr] <- ifelse(ans[rr] == "", "Gamlársdagur", paste(ans[rr], "Gamlársdagur", sep = ", "))
    }
  } else {
    ans <- rep(FALSE, NROW(x))
    if (weekend) ans <- ans | as.POSIXlt(x)$wday == 6 | as.POSIXlt(x)$wday == 0
    if (days == "all" || days == "whole") {
      ans <- ans |
        xx == "01.01"      | # 1. janúar, nýársdagur
        x == paskar - 3    | # Skírdagur
        x == paskar - 2    | # Föstudagurinn langi
        x == paskar        | # Páskadagur
        x == paskar + 1    | # Annar í páskum
        x == sumardagur    | # Sumardagurinn fyrsti
        xx == "01.05"      | # 1. maí
        x == paskar + 39   | # Uppstigningardagur
        x == paskar + 49   | # Hvítasunnudagur
        x == paskar + 50   | # Annar í hvítasunnu
        xx == "17.06"      | # 17. júní
        x == versldagur    | # Frídagur verslunarmanna
        xx == "25.12"      | # 25. desember
        xx == "26.12"        # 26. desember
    }
    if (days == "all" || days == "half")
    {
      ans <- ans |
        xx == "24.12"      | # 24. desember frá kl. 13
        xx == "31.12"        # 31. desember frá kl. 13
    }
  }
  ans
}

getVinnustundir <- function(y, breakdown="%Y", fullhours = 8, halfhours = 4, weekendhours = 0) {
  # Get the number of working hours in year 'y', broken down into 'breakdown', given
  # the number of hours worked in a full day, half day and at weekends.
  # Example:
  #   getVinnustundir(2013)
  #   getVinnustundir(1989:2013)
  #   getVinnustundir(1997:2011, "%Y-%m", 8, 4, 0)
  #   getVinnustundir(1997:2011, "%Y", 12, 8, 8)
  #
  if (length(y) > 1) {
    theDates <- do.call("c", lapply(y, function(x) seq(as.Date(paste(x, 1, 1, sep="/")), as.Date(paste(x, 12, 31, sep="/")), by="1 day")))
  } else {
    theDates <- seq(as.Date(paste(y, 1, 1, sep="/")), as.Date(paste(y, 12, 31, sep="/")), by="1 day")
  }
  weekend <- getFridagur(theDates, days="none", weekend=TRUE)
  fullday <- !getFridagur(theDates, days="all", weekend=TRUE)
  halfday <- getFridagur(theDates, days="half", weekend=FALSE) & !weekend
  tapply(
    fullhours * fullday + halfhours * halfday + weekendhours * weekend, 
    format(theDates, breakdown), 
    FUN=sum)
}

