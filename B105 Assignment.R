dim(train)
names(train)
str(train)
head(train) 
colSums(is.na(train))
sum(duplicated(train))
library("tidyverse")
train_clean <- train %>%
  drop_na() %>%
  distinct()

category_sales <- train_clean %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Average_Sales = mean(Sales, na.rm = TRUE),
    Order_Count = n()
  ) %>%
  arrange(desc(Total_Sales))
print(category_sales)
ggplot(category_sales, aes(x = reorder(Category, Total_Sales), y = Total_Sales, fill = Category)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Sales Revenue by Product Category",
    x = "Product Category",
    y = "Total Sales ($)"
  ) +
  theme_minimal()

region_sales <- train_clean %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Average_Sales = mean(Sales, na.rm = TRUE),
    Order_Count = n()
  ) %>%
  arrange(desc(Total_Sales))
print(region_sales)

ggplot(region_sales, aes(x = reorder(Region, Total_Sales), y = Total_Sales, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Sales Revenue by Region",
    x = "Region",
    y = "Total Sales ($)"
  ) +
  theme_minimal()

ship_mode_sales <- train_clean %>%
  group_by(Ship.Mode) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Average_Sales = mean(Sales, na.rm = TRUE),
    Order_Count = n()
  ) %>%
  arrange(desc(Total_Sales))
print(ship_mode_sales)

ggplot(ship_mode_sales, aes(x = reorder(Ship.Mode, Total_Sales), y = Total_Sales, fill = Ship.Mode)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Sales Revenue by Shipping Mode",
    x = "Shipping Mode",
    y = "Total Sales ($)"
  ) +
  theme_minimal()

anova_model <- aov(Sales ~ Ship.Mode, data = train_clean)

# View the ANOVA summary table (p-values, F-statistics)
summary(anova_model)

# Check model assumptions (Normality of residuals / diagnostics)
par(mfrow = c(2, 2))
plot(anova_model)
par(mfrow = c(1, 1))
