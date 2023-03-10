
install <- function(pkg){
  new.pkg <- packages[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("data.table", "RJDBC", "dplyr", "ggplot2", "gridExtra",
              "wesanderson", "lubridate", "scales", "ggrepel", "leaflet")
packages <- c("data.table", "RJDBC", "dplyr", "ggplot2", "stringr", "RColorBrewer", "tibble", "leaflet")
packages <- c("data.table", "RJDBC", "dplyr", "ggplot2", "gridExtra", "scales")
packages <- c("leaflet")
install(packages)

conn <- hive2_connection_saturn_tez(user='t1111594')
conn = hive2_connection_saturn(hostname='snn-06-01.sktelecom.com', user='t1111594')

# Open Street Map (URL Template)
u1 = "http://150.6.14.131:8888/proxy?type=image&link=https://tile.openstreetmap.org/{z}/{x}/{y}.png"
u2 = "http://150.6.14.131:8888/proxy?type=image&link=https://tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
u3 = "http://150.6.14.131:8888/proxy?type=image&link=https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png"

#############################################################################################################
# 퍼져나가는 효과
#############################################################################################################

sig = dbGetQuery(conn, "select * from di_crowd.realtor_weekly_trend_change 
                 where week_start_dt >= '20220801' AND district_type = 'SIG'")

sig_list = c('강남구', '성남시 분당구', '연수구', '오산시')
sig %>% filter(sig_name %in% sig_list) %>%
  mutate(sig_name = factor(sig_name, levels=sig_list)) %>% 
  filter(week_start_dt >= '20230108') %>%
  group_by(week) %>% arrange(desc(med_voc_change_half_year)) %>% mutate(ranking = row_number()) %>% 
  mutate(a = ifelse(ranking == 1, 'T', 'F')) %>% 
  ggplot() +
  geom_bar(aes(x=week, y=med_voc_change_half_year, group=sig_name, fill=sig_name, alpha=a), stat="identity", position=position_dodge()) +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  guides(alpha = "none") +
  scale_alpha_continuous(range = c(0.2, 1)) +
  xlab("시점") + 
  ylab("부동산 통화량 26주 변화율(%)") 

#############################################################################################################
# 1기 신도시
#############################################################################################################

jae = dbGetQuery(conn, "select * from di_crowd.realtor_daily_cdr_summary
                 where district_type IN ('SIG', 'CT') and exec_dt >= '20230201' ")

sig_list = c('고양시 일산서구', '성남시 분당구', '안양시 동안구', '군포시', '부천시')
plot = jae %>% filter((sig_name %in% sig_list)) %>%
  mutate(sig_name = factor(sig_name, levels=sig_list)) %>% 
  filter(week_start_dt <= '20230219') %>%
  ggplot() +
  geom_line(aes(x=exec_dt, y=med_voc_eff_count, group=sig_name, color=sig_name), size=1) +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  guides(alpha = "none") + 
  xlab("") + ylab("부동산 통화량 중앙값(건)") +
  geom_vline(xintercept="20230207", lty="dashed"); plot

gyeonggi = jae %>% filter(ct_name == '경기' & district_type == 'CT') %>% filter(week_start_dt <= '20230219')
plot +
  geom_bar(data = gyeonggi, aes(x=exec_dt, y=med_voc_eff_count), stat="identity", alpha=0.1) +
  theme(legend.position="top")



#############################################################################################################
# 송파구 안전진단
#############################################################################################################
j = dbGetQuery(conn, "select * from di_crowd.realtor_daily_cdr_summary
                 where district_type IN ('SIG', 'CT', 'EMD') and exec_dt >= '20230201' ")
col1 = c('#EFC959', '#E9B045', '#E55D3D', '#CD483A', '#9F2E2C')

j %>% filter(district_type=='SIG' & sig_name=='송파구') %>%
  ggplot() +
  geom_bar(aes(x=exec_dt, y=med_voc_eff_count), stat="identity", fill=col1[2]) +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_fill_manual(values=col1[2]) +
  geom_vline(xintercept="20230219", lty="dashed") +
  xlab("") + ylab("부동산 통화량 중앙값(건)")
 

j %>% filter(district_type=='EMD' & sig_name=='송파구') %>%
  mutate(a = ifelse(emd_name %in% c('풍납동', '방이동', '송파동'), '풍납동/방이동/송파동', '비 풍납동/방이동/송파동')) %>%
  ggplot() +
  geom_bar(aes(x=exec_dt, y=med_voc_eff_count, fill=a, group=a), stat="identity") +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  geom_vline(xintercept="20230219", lty="dashed") +
  scale_fill_manual(values=c('gray40', col1[2])) +
  xlab("") + ylab("부동산 통화량 중앙값(건)")



#############################################################################################################
# 특례보금자리론
#############################################################################################################
sig = dbGetQuery(conn, "select * from di_crowd.realtor_weekly_trend_change 
                 where week_start_dt >= '20230101' AND district_type IN ('SIG', 'CT')")

# 전국
sig %>% filter(district_type == 'CT' & ct_name != '세종') %>%
  mutate(period = paste0(as.numeric(substr(week_start_dt, 5, 6)), '/', as.numeric(substr(week_start_dt, 7, 8)), '-',  
                         as.numeric(substr(week_end_dt, 5, 6)), '/', as.numeric(substr(week_end_dt, 7, 8)))) %>%
  ggplot() +
  geom_tile(aes(x=reorder(period, as.numeric(week_start_dt)), y=ct_name, fill=med_voc_change_half_year )) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC",high = "#FF0000") +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("시군구") + xlab("기간") 

# 서울 시군구
sig %>% filter(district_type == 'SIG' & ct_name == '서울') %>%
  mutate(period = paste0(as.numeric(substr(week_start_dt, 5, 6)), '/', as.numeric(substr(week_start_dt, 7, 8)), '-',  
                         as.numeric(substr(week_end_dt, 5, 6)), '/', as.numeric(substr(week_end_dt, 7, 8)))) %>%
  ggplot() +
  geom_tile(aes(x=reorder(period, as.numeric(week_start_dt)), y=sig_name, fill=med_voc_change_half_year )) +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC",high = "#FF0000") +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("시군구") + xlab("기간") 



#############################################################################################################
# 연수구
#############################################################################################################


## 연수구 20대 비율
seg = dbGetQuery(conn, "select * from di_crowd.realtor_seg_weekly_prtn where district_type='SIG' and sig_name='연수구'")
tb = seg %>% group_by(age_grp, week, week_start_dt, week_end_dt) %>% summarise(svc_count = sum(svc_count), rate=sum(rate)) %>% ungroup() %>% 
  filter(age_grp %in% c('20', '30', '40', '50', '60', '70_over')) %>%
  mutate(age_grp = paste0(age_grp, '대')) 

tb2 = seg %>% group_by(age_grp, week, week_start_dt, week_end_dt) %>% summarise(svc_count = sum(svc_count), rate=sum(rate)) %>% ungroup() %>% 
  filter(age_grp %in% c('20')) %>%
  group_by(week, week_start_dt) %>% summarise(rate = sum(rate))

m1 = max(tb2$rate); m2 = max(tb$svc_count); r=3; m1;m2

tb %>% ggplot() +
  geom_bar(data=tb, aes(x=week_start_dt, y=svc_count, fill=age_grp, group=age_grp), stat="identity") +
  geom_line(data=tb2, aes(x=week_start_dt, y=rate/m1*m2*r, group=1), size=2) +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("") + ylab("통화량(건)") +
  scale_fill_manual(values=palette('set2')) +
  scale_y_continuous(sec.axis = sec_axis(~.*m1/m2*r, name = "20대 통화량 비율(%)")) +
  facet_grid(~substr(week_start_dt, 1, 4), scales="free_x", space="free")



## 강남구 20대 비율
seg = dbGetQuery(conn, "select * from di_crowd.realtor_seg_weekly_prtn where district_type='SIG'")

s = '강남구'
tb = seg %>% filter(sig_name == s) %>% distinct(week, week_start_dt, total_svc_count)

tb2 = seg %>% filter(sig_name == s) %>% 
  filter(age_grp %in% c('30', '70_over')) %>% 
  group_by(age_grp, week, week_start_dt, week_end_dt) %>% 
  mutate(rate=sum(rate))

m1 = max(tb2$rate); m2 = max(tb$total_svc_count); r=1; m1;m2

tb %>% ggplot() +
  geom_bar(data=tb, aes(x=week_start_dt, y=total_svc_count), stat="identity", alpha=0.3) +
  geom_line(data=tb2, aes(x=week_start_dt, y=rate/m1*m2*r, group=age_grp, col=age_grp), size=2.2) +
  theme(strip.background = element_rect(color="black", fill='white'),
        panel.background = element_rect(color="black", fill='white'),
        legend.position = 'top', 
        legend.title = element_blank(),
        strip.text = element_text(size=10, face='bold'),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("") + ylab("통화량(건)") +
  scale_fill_manual(values=palette('set2')) +
  scale_y_continuous(sec.axis = sec_axis(~.*m1/m2*r, name = "연령대별 통화 비율(%)")) +
  facet_grid(~substr(week_start_dt, 1, 4), scales="free_x", space="free")

