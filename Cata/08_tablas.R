#####TABLAS

#A6.2
data_isp_rc %>% count(a6.2)

pond_1 <- data_isp_c %>% pull(pond1)

T1.1<-freq(data_isp_c$a6.2, weights = data_isp_c$pond1, useNA = c("no"),  round.digits = 2)
T1.2<-freq(data_isp_c$a0_pais, data_isp_c$a6.2, weights = data_isp_c$pond_1, useNA = c("no"),  round.digits = 2)
T1.3<-ctable(data_isp_c$a6.2, data_isp_c$a1_sexo, weights = pond_1, round.digits = 2)
T1.4<-ctable(data_isp_c$a6.2, data_isp_c$a5_etnia , weights = pond_1, round.digits = 2)
T1.5<-ctable(data_isp_c$a6.2, data_isp_c$modalidad , weights = pond_1, round.digits = 2)

#table_freq_01 <- mergesvy %>%
#  dplyr::group_by(a6.2, a0_pais) %>%
#  summarize(proportion = survey_mean(,na.rm=TRUE)) 
##help(srvyr)
#
#df=data.frame(table_freq_01, digits=2)
##summary(factor(df$proportion))
#
#df= df %>%
#  mutate(perc = proportion * 100) %>%
#  mutate(p=round(perc, 2))
#
#
#df %>% 
#  ggplot(aes(x=a6.2, y=proportion))+
#  geom_bar(stat="identity", position = position_dodge(), fill="#d3d3d3")+
#  geom_text(aes(label = p), position = position_dodge(0.8), vjust=0, hjust = -0.1, size = 4, angle =90)+
#  theme(axis.text.x = element_text(angle =90, hjust = 1)) +
#  facet_wrap(~ a0_pais) +
#  labs(x = "Recursos económicos en el hogar", y = "%") +
#  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent)


