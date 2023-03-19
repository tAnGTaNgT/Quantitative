library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)


load('mlfactor.github.io-master/material/data_ml.RData')


stock_ids <- levels(as.factor(data_ml$stock_id)) # A list of all stock_ids
stock_days <- data_ml %>%                        # Compute the number of data points per stock
  group_by(stock_id) %>% summarize(nb = n()) 
stock_ids_short <- stock_ids[which(stock_days$nb == max(stock_days$nb))] # Stocks with full data
returns <- data_ml %>%                           # Compute returns, in matrix format, in 3 steps:
  filter(stock_id %in% stock_ids_short) %>%    # 1. Filtering the data
  dplyr::select(date, stock_id, R1M_Usd) %>%   # 2. Keep returns along with dates & firm names
  spread(key = stock_id, value = R1M_Usd)      # 3. Put in matrix shape 


data_ml %>% 
  group_by(date) %>% 
  mutate(large = Mkt_Cap_12M_Usd > median(Mkt_Cap_12M_Usd)) %>% 
  ungroup() %>% 
  mutate(year = lubridate::year(date)) %>%  
  group_by(year, large) %>% 
  summarize(avg_return = mean(R1M_Usd)) %>% 
  ggplot(aes(x = year, y = avg_return, fill = large)) +         # Plot!
  geom_col(position = "dodge") + theme_light() +                # Bars side-to-side
  theme(legend.position = c(0.8, 0.2)) +                        # Legend location
  coord_fixed(124) + theme(legend.title=element_blank()) +      # x/y aspect ratio
  scale_fill_manual(values=c("#F87E1F", "#0570EA"), name = "",  # Colors
                    labels=c("Small", "Large"))  +
  ylab("Average returns") + theme(legend.text=element_text(size=9)) 


data_ml %>% 
  select(R1M_Usd, stock_id, date) %>% 
  filter(stock_id == 13) %>% 
  group_by(stock_id) %>% 
  mutate(R1M_Usd_ = lag(R1M_Usd))


library(quantmod)                         # Package for data extraction
library(xtable)                           # Package for LaTeX exports 
min_date <- "1963-07-31"                  # Start date
max_date <- "2020-03-28"                  # Stop date
temp <- tempfile()
KF_website <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
KF_file <- "ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
link <- paste0(KF_website, KF_file)       # Link of the file
download.file(link, temp, quiet = TRUE)   # Download!
FF_factors <- read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3.csv"), 
                       skip = 3) %>%          # Check the number of lines to skip!
  rename(date = `...1`, MKT_RF = `Mkt-RF`) %>%  # Change the name of first columns
  mutate_at(vars(-date), as.numeric) %>%                 # Convert values to number
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>%  # Date in right format
  mutate(date = rollback(date + months(1)))              # End of month date

FF_factors <- FF_factors %>% mutate(MKT_RF = MKT_RF / 100, # Scale returns
                                    SMB = SMB / 100,
                                    HML = HML / 100,
                                    RMW = RMW / 100,
                                    CMA = CMA / 100,
                                    RF = RF/100) %>%
  filter(date >= min_date, date <= max_date)             # Finally, keep only recent points




knitr::kable(head(FF_factors),  booktabs = TRUE,
             caption = "Sample of monthly factor returns.") # A look at the data (see table)      


nb_factors <- 5                                                     # Number of factors
data_FM <- left_join(data_ml %>%                                    # Join the 2 datasets
                       dplyr::select(date, stock_id, R1M_Usd) %>% # (with returns...
                       filter(stock_id %in% stock_ids_short),     # ... over some stocks)
                     FF_factors, 
                     by = "date") %>% 
  group_by(stock_id) %>%                                          # Grouping
  arrange(date) %>% 
  mutate(R1M_Usd = lag(R1M_Usd)) %>%                              # Lag returns
  ungroup() %>%
  na.omit() %>%                                                   # Remove missing points
  pivot_wider(names_from = "stock_id", values_from = "R1M_Usd")

models <- lapply(paste0("`", stock_ids_short, 
                        '` ~  MKT_RF + SMB + HML + RMW + CMA'),           # Model spec
                 function(f){ lm(as.formula(f), data = data_FM,           # Call lm(.)
                                 na.action="na.exclude") %>%       
                     summary() %>%                                    # Gather the output
                     "$"(coef) %>%                                    # Keep only coefs
                     data.frame() %>%                                 # Convert to dataframe
                     dplyr::select(Estimate)}                         # Keep the estimates
)
betas <- matrix(unlist(models), ncol = nb_factors + 1, byrow = T) %>%     # Extract the betas
  data.frame(row.names = stock_ids_short)                               # Format: row names
colnames(betas) <- c("Constant", "MKT_RF", "SMB", "HML", "RMW", "CMA")    # Format: col names


betas




