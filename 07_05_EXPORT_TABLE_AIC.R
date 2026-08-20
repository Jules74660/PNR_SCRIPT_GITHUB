# Récupérer et arrondir tes 4 tableaux
tab1 <- as.data.frame(aictab(list(fm0, fm1, fm2, fm3, fm4, fm5, fm6, fm7),
         modnames = c("null","OBS","HEURE","DATE","FF","NBR_MAR","RR1","full")))

tab2 <- as.data.frame(aictab(list(fm7, fm5, fm_nbrmar_obs, fm_nbrmar_heure, fm_nbrmar_date, fm_nbrmar_ff, fm_nbrmar_rr1),
         modnames = c("full","nbr_mar","NBR_MAR+obs","NBR_MAR+heure","NBR_MAR+date","NBR_MAR+ff","NBR_MAR+rr1")))

tab3 <- as.data.frame(aictab(list(fm_occ_null, fm_occ_alti, fm_occ_marmax, fm_occ_mar50, fm_occ_ouvrage,
         fm_occ_f100, fm_occ_dh500, fm_occ_dr100, fm_occ_dr500, fm_occ_rive),
         modnames = c("null","alti","marmax","mar50","ouvrage","f100","dh500","dr100","dr500","rive")))

tab4 <- as.data.frame(aictab(list(fm_occ_marmax, fm_occ_marmax_alti, fm_occ_marmax_ouvrage, fm_occ_marmax_f100,
         fm_occ_marmax_dr100, fm_occ_marmax_dr500, fm_occ_marmax_rive),
         modnames = c("marmax","marmax+alti","marmax+ouvrage","marmax+f100","marmax+dr100","marmax+dr500","marmax+rive")))

tab5 <- as.data.frame(aictab(list(fm_occ_marmax_alti, fm_MARMAX_log),modnames = c("MAR_MAX linéaire", "log(MAR_MAX + 1)")))

for (t in c("tab1","tab2","tab3","tab4", "tab5")) {
  df <- get(t)
  df[ ,-1] <- round(df[ ,-1], 2)
  assign(t, df)
}

library(gridExtra)
pdf("AIC_occup_glu.pdf", width = 10, height = 6)

grid.table(tab1)
grid.newpage()
grid.table(tab2)
grid.newpage()
grid.table(tab3)
grid.newpage()
grid.table(tab4)
grid.newpage()
grid.table(tab5)

dev.off()
