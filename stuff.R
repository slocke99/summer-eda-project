#stuff

#Load Data----
library(tidyverse)
medicare_prescriptions_data <-read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/medicare_partd_presc_claims.csv")
head(medicare_prescriptions_data, 6)

table(medicare_prescriptions_data$Specialty)

#medicare_prescriptions_data <- write_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/medicare_partd_presc_claims.csv")

medicare_prescriptions_data %>%
  ggplot(aes(x= Specialty))+
  geom_bar()+
  theme_bw()
#Clean Data
Clean_medicare <- medicare_prescriptions_data %>%
  filter(State == "TX", !is.na(TotalDrugCost),!is.na(NumberMedicareBeneficiaries)) %>%
  mutate(log_cost = log(TotalDrugCost), log_benefic = log(NumberMedicareBeneficiaries))%>%
  mutate(std_log_cost = as.numeric(scale(log_cost, center = TRUE, scale = TRUE)),
         std_num_medben = as.numeric(scale(log_benefic, center = TRUE, scale = TRUE)))
#Initial K Means clustering----
init_kmeans <- 
  kmeans(dplyr::select(Clean_medicare,
                       log_cost, NumberMedicareBeneficiaries),
         algorithm = "Lloyd", centers = 4)
#Standardized K Mean Cluster
std_kmeans <- 
  kmeans(dplyr::select(Clean_medicare,
                       std_log_cost, std_num_medben),
         algorithm = "Lloyd", centers = 4)
# Scatterplot of claims vs drugs cost vs fills for Pennsylvania----
Clean_medicare %>%
  ggplot(aes(x=(log_cost), y=NumberMedicareBeneficiaries))+
  scale_color_viridis_c()+
  geom_point(alpha=.25)+
  theme_bw()
#Clustering plot----
Clean_medicare %>% 
 mutate(cost_clusters = as.factor(init_kmeans$cluster))%>%
  ggplot(aes(x= log_cost, y=NumberMedicareBeneficiaries, color=cost_clusters))+
  ggthemes::scale_color_colorblind()+
  geom_point(alpha=.25)+
  theme_bw()+
  theme(legend.position = "bottom")+
  coord_fixed()

#Clustering plot wwith standarized values
Clean_medicare %>%
  mutate(cost_clusters = 
           as.factor(std_kmeans$cluster)) %>%
  ggplot(aes(y= std_log_cost, x=std_num_medben, color=cost_clusters)) +
  geom_point(alpha=0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8)) +
  labs(title="Clustering Medicare Drug Prescriptions by Total Cost and Number of Medicare Beneficaries(TX)"
       ,y="Total Cost",x="# of Medicare Beneficaries", color ="Prescription Clusters")+
  coord_fixed()
#arrange data and create bar graph for states----
Cost_States <- medicare_prescriptions_data %>%
  select(TotalDrugCost) %>%
  group_by( state = medicare_prescriptions_data$State)%>%
  summarize(meddrugcost= median(TotalDrugCost, na.rm = TRUE))
head(medicare_prescriptions_data)

head(medicare_prescriptions_data)
#Barplot measuring state vs cost
Cost_States %>%
  ggplot(aes(x=reorder(state,- meddrugcost),y= meddrugcost))+
  geom_bar(stat = "identity")+
  theme_bw()
#Visualize data on a map
library(usmap)
plot_usmap(data=Cost_States, values="meddrugcost",color="red")+
  scale_fill_continuous(name= "Median Drug Cost",low = "white",
                        high = "red",label= scales::comma)+
  theme(legend.position = "right")
  


Cost_States %>%
  ggplot(aes(x= avgdrugcost))+
  geom_histogram()

#Viluaze Drug Used

medicare_prescriptions_data %>%
  ggplot(aes(x=TotalDrugCost))+
  geom_histogram()+
  theme_bw()
table(medicare_prescriptions_data$BrandName)

#Why Mean cost doesnt work
medicare_prescriptions_data %>%
  ggplot(aes(x= State))+
  geom_bar()+
  theme_bw()

#Barplot proportion of opiod prescription by speciality
medicare_prescriptions_data %>%
  ggplot(aes(x= fct_infreq(SpecialtyCateg), fill= OpioidFlag))+
  geom_bar(position = "dodge")+
  theme_bw()



