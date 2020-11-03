libs = c("rtweet")
lapply(libs, require, character.only = TRUE)
rm(libs)

# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

rt = search_tweets("moose", n = 18000, include_rts = FALSE, geocode = lookup_coords("alaska, usa"))
rt
users_data(rt)
ts_plot(rt)