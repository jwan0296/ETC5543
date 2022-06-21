seri<-referrals_clean3%>%filter(!is.na(date_received))
seri%>%
  group_by(month = yearmonth(date_received))%>%
  summarise(referrals=n())
seri_month<-seri%>%
  as_tsibble(index = month)
seri%>%duplicates()
seri<-seri%>%
  filter(!is.na(date_received))%>%
  tsibble(index = date_received)
p1<-seri%>%filter(date_received>"2022-01-01")%>%autoplot()
p1
seri_month%>%gg_season()
seri_month%>%autoplot()

seri%>%
  group_by(as_da)