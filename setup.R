clean = function(x, digits = 0, prefix = "", suffix = "") {
    paste0(prefix, format(x, digits = digits, big.mark = ",", big.interval = 3, scientific = FALSE), suffix)
}

pmt = function(principal, rate, term = 30, period = 12) {
    rateP = rate / period
    n = term * period
    principal * rateP / (1 - (1 + rateP) ^ -n)
}


reducePrincipal = function(principal, rate, term = 30, period = 12, currentYear = 1) {
    rateP = rate / period
    v1 = principal * (1 + rateP) ^ (period * currentYear)
    v2 = (1 + rateP) ^ (period * currentYear) - 1
    mthly = pmt(principal = principal, rate = rate, term = term, period = period)
    v3 = mthly / rateP * v2
    v1 - v3
}


getAnalyses = function(path = "data") {
    list.files(path) %>%
        map_df(function(x) {
            m = readRDS(file = file.path(path, x))
            m$analysisId = as.character(m$analysisId)
            m
        }) %>%
        arrange(desc(analysisId))
}


# Horizontal inputs with units
hinput <- function(label, input, unit) {
    div(
        div(label, class = "hinput-label"),
        div(input, class = "hinput-input"),
        div(unit, class = "hinput-unit"),
        class = "hinput"
    )
}

