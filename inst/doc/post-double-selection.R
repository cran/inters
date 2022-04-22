## ----loadpkg, echo = FALSE, include = FALSE-----------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5, fig.align = "center")
require(ggplot2)
library(lmtest)
library(fixest)
library(inters) 

## ----single-------------------------------------------------------------------
data(remit)
single <- feols(Protest ~ remit*dict + l1gdp + l1pop + l1nbr5 + l12gr + l1migr
               + elec3 | period + cowcode, data = remit)
coeftable(single, cluster = ~ caseid)[c("remit", "remit:dict"),]

## ----fully--------------------------------------------------------------------
fully <- feols(Protest ~ dict * (remit + l1gdp + l1pop + l1nbr5 + l12gr + l1migr +
                                   elec3 + factor(period) + factor(cowcode)),
               data = remit)
coeftable(fully, cluster ~ caseid)[c("remit", "dict:remit"),]

## ----post-double--------------------------------------------------------------
controls <- c("l1gdp", "l1pop", "l1nbr5", "l12gr", "l1migr", "elec3")
post_ds_out <- post_ds_interaction(data = remit, treat = "remit", moderator = "dict",
                                outcome = "Protest", control_vars = controls,
                                panel_vars = c("cowcode", "period"),
                                cluster = "caseid")
lmtest::coeftest(post_ds_out, vcov = post_ds_out$clustervcv)[c("remit", "remit_dict"),]

## ----calcs, echo = FALSE------------------------------------------------------
cis <- rbind(coefci(single, parm = c("remit", "remit:dict"),
                    vcov = single$clustervcv),
             coefci(fully, parm = c("remit", "dict:remit"),
                    vcov = fully$clustervcv),
             coefci(post_ds_out, parm = c("remit", "remit_dict"),
                    vcov = post_ds_out$clustervcv))
colnames(cis) <- c("lower", "upper")
coefs <- c(coef(single)[c("remit", "remit:dict")],
           coef(fully)[c("remit", "dict:remit")],
           coef(post_ds_out)[c("remit", "remit_dict")])
coefficients_df <- data.frame(
  qoi  = factor(rep(c("Marginal Effect, Democracy",
                 "Interaction"), times = 3)),
  method =  factor(rep(c("Single Interaction", "Fully Moderated",
                  "Post-double Selection"), each = 2)),
  est = coefs,
  cis = cis
)

## ----coefplot, echo = FALSE---------------------------------------------------
ggplot(coefficients_df, aes(x = qoi, y = est,
                            group = method, colour = method,
                            shape = method)) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, colour = "indianred") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(size = 1, aes(ymin = cis.lower, ymax = cis.upper),
                position = position_dodge(width = 0.5), width = 0) +
  scale_colour_grey() +
  xlab("Quantity of Interest") +
  labs(color = "Method", shape = "Method") +
  ylab("Estimate") +
  theme_minimal()

