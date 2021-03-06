library(testthat)
library(ggplot2)
library(noaayd)

expect_is(eq_clean_data(rawdata), "data.frame")
expect_is(eq_clean_data(rawdata)$DATE, "Date")
expect_is(eq_clean_data(rawdata)$LATITUDE, "numeric")
expect_is(eq_clean_data(rawdata)$LONGITUDE, "numeric")
expect_is(eq_location_clean(rawdata), "data.frame")
expect_is(ggplot(cleandata_2011, aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS)) + geom_timeline(alpha = 0.5), "ggplot")
expect_is(ggplot(cleandata_2011, aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS, label = LOCATION_NAME)) + geom_timeline_label(alpha = 0.5, n_max=5), "ggplot")
expect_is(eq_map(data = cleandata_MEXICO2000, annot_col = "DATE"), "leaflet")
expect_is(eq_create_label(data = cleandata_MEXICO2000), "character")
expect_equal(length(eq_create_label(data = cleandata_MEXICO2000)), nrow(cleandata_MEXICO2000))



