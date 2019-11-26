

load_all()

data("stan_dat")


ea_countries <- eurostat::ea_countries$code
eu_countries<- eurostat::eu_countries$code

#9 other industrial countries (Australia, Canada, United States, Japan, Norway, New Zealand, Mexico, Switzerland and Turkey)
IC37_other <- c("AU", "CA", "US", "JP", "NO", "NZ", "MX", "CH", "TR")
exec_countries <- c("CL", "CR", "IL", "IS", "KR")
other_oecd <- c("AU", "CA", "US", "JP", "NO", "NZ", "CH")

setdiff(unique(stan_dat$geo), eurostat::eu_countries$code)

setdiff(eurostat::eu_countries$code, unique(stan_dat$geo))

eurostat::label_eurostat(kk, dic = "geo")
eurostat::label_eurostat(IC37_other, dic = "geo")
