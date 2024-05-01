# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
library(readxl)
# 2.0 Importing Files ----

bikes_tbl      <- read_excel(path = "00_data/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_raw_data/orderlines.xlsx")

# Not necessary for this analysis, but for the sake of completeness
bikeshops_tbl  <- read_excel("00_data/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----



# 4.0 Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

library(lubridate)

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  #select columns
  select(order_date,total_price) %>%
  
  #add year column
  mutate(year = year(order_date)) %>%
  
  #group by year & summarise sales
  
  group_by(year)%>%
  summarise(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_tbl

# Step 2 - Visualize

sales_by_year_tbl %>%
  
  ggplot(aes(x=year,y=sales))+
  
  geom_col(fill="#2DC6D6")+
  geom_label(aes(label = sales_text))+
  geom_smooth(method="lm",se=FALSE)+
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
  

# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(year,category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales,big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

sales_by_year_cat_1_tbl

# Step 2 - Visualize

sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.1 Excel ----
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/02_wrangled_data/bike_orderlines.rds")


#CHALLENGES

#1: Analyse sales by location with bar plot


bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  # Separate location into city and state
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ")

library(lubridate)

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  #select columns
  select(total_price, state) %>%
  
  
  #group by year & summarise sales
  
  group_by(state)%>%
  summarise(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_state_tbl

# Step 2 - Visualize

sales_by_state_tbl %>%
  
  ggplot(aes(x=state,y=sales)) +
  
  geom_col(fill="#2DC6D6")+
  geom_label(aes(label = sales_text))+
  geom_smooth(method="lm",se=FALSE)+
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by State",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )+
  
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2: analyse sales by location and year(facet_wrap)


  #Step 1: Manipulate

sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(year,state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales,big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

sales_by_year_state_tbl

# Step 2: Visualise

sales_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and State",
    fill = "State" # Changes the legend name
  )

