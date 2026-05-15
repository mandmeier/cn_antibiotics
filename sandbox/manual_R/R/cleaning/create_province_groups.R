# Table 12 from Qu et al (2024)
#Qu, X., Zhang, Y., & Li, Z. (2024). Is china’s rural revitalization good enough? Evidence from Spatial #agglomeration and cluster analysis. Sustainability, 16(11), 4574.
# https://www.mdpi.com/2071-1050/16/11/4574
# 31 provinces of China could be clustered into four categories and seven subcategories through Ward cluster analysis on the rural revitalization (RR) secondary index. Each province in the different subcategories is facing different challenges in the process of RR and entails different development focuses.



# Create dataframe with province clustering data
province_groups <- data.frame(
  Province = c("Beijing", "Shanghai", "Zhejiang", "Tianjin",
               "Jiangsu", "Shandong", "Fujian",
               "Anhui", "Chongqing", "Hubei",
               "Qinghai", "Tibet",
               "Guangdong", "Hunan", "Henan", "Hebei",
               "Gansu", "Yunnan", "Guizhou", "Ningxia", "Sichuan",
               "Xinjiang", "Jiangxi", "Guangxi", "Jilin",
               "Inner Mongolia", "Shaanxi", "Shanxi", "Hainan",
               "Heilongjiang", "Liaoning"),

  Category = c("First", "First", "First", "First",
               "Second", "Second", "Second",
               "Second", "Second", "Second",
               "Third", "Third",
               "Fourth", "Fourth", "Fourth", "Fourth",
               "Fourth", "Fourth", "Fourth", "Fourth", "Fourth",
               "Fourth", "Fourth", "Fourth", "Fourth",
               "Fourth", "Fourth", "Fourth", "Fourth",
               "Fourth", "Fourth"),

  Subcategory = c(1, 1, 1, 1,
                  2, 2, 2,
                  3, 3, 3,
                  4, 4,
                  5, 5, 5, 5,
                  6, 6, 6, 6, 6,
                  6, 6, 6, 6,
                  7, 7, 7, 7,
                  7, 7),

  Region = c("Eastern", "Eastern", "Eastern", "Eastern",
             "Eastern", "Eastern", "Eastern",
             "Central", "Western", "Central",
             "Western", "Western",
             "Eastern", "Central", "Central", "Eastern",
             "Western", "Western", "Western", "Western", "Western",
             "Western", "Central", "Western", "Northeastern",
             "Western", "Western", "Central", "Eastern",
             "Northeastern", "Northeastern"),

  Ranking = c(4, 2, 5, 7,
              1, 6, 3,
              8, 10, 9,
              23, 17,
              11, 13, 15, 14,
              24, 20, 22, 16, 19,
              18, 12, 26, 21,
              25, 27, 30, 31,
              29, 28),

  stringsAsFactors = FALSE
)



colnames(province_groups) <- c("province", "qu_category", "qu_subcategory", "NBS_region", "RR_ranking")



write_csv(province_groups, "data/cleaned/province_groups.csv")



print("K-means clusters added in script province_groups_by_antibiotics.R")

