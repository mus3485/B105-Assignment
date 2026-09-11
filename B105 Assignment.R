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

