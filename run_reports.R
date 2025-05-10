d$truncated="not truncated"
fit <- fit_mixture(z=d$z, truncated=d$truncated, k=4, weight=1/d$k)

knit_with_df(d, fit)
stitch_and_knit("brodeur_desc.Rmd", "template.Rmd", d, fit, "template_stitch.pdf")
