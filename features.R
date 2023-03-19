library(Tushare)
library(stringr)


api <- Tushare::pro_api(token = 'b9d8623c7dbde160e75147e2b47588810dd8dc3033ec8cadc9f64f53')

data <- api(api_name = 'stock_basic',
            # exchange = 'SZSE', 
            list_status = 'L', 
            fields='ts_code,symbol,name,area,industry,list_date')

data %>% 
  filter(str_detect(name, '通策'))

colSums(is.na(data))


# 鱼跃医疗、乐普医疗、三诺生物、阳普医疗、通策医疗、新华医疗
ts_codes <- c('002223.SZ', '300003.SZ', '300298.SZ', 
              '300030.SZ', '600763.SH', '600587.SH')



fin_report_record <- function(df, record_date) {
  df <- df %>% 
    filter(end_date == record_date)
  
  if(nrow(df) > 1) {
    df <- df[df$update_flag == '1', ]
  }
  
  df
}

record_date <- '20201231'

for (ts_code in ts_codes) {
  balance <- api(api_name = 'balancesheet', 
                 ts_code = ts_code, 
                 start_date = '20200101',
                 end_date = '20211231')
  
  
  balance <- fin_report_record(balance, record_date)
  
  
  income <- api(api_name = 'income',
                ts_code = ts_code,
                start_date = '20200101', 
                end_date = '20211231')
  
  income <- fin_report_record(income, record_date)
  
  # 流动比率 流动资产/流动负债
  f <- round(balance$total_cur_assets / balance$total_cur_liab, 2)
  print(f)
  
  # 产权比率 总负债/（普通股）所有者权益
  oth_eqt_tools_p_shr <- if_else(is.na(balance$oth_eqt_tools_p_shr), 0, balance$oth_eqt_tools_p_shr)
  f <- round(balance$total_liab / (balance$total_hldr_eqy_inc_min_int - oth_eqt_tools_p_shr), 2)
  print(f)
  
  print('*********************************')
  
  
  # 存货周转率 销售成本 / 平均存货  利润表和资产负债表
  in1.oper_cost.tolist()[0] / ((b1.inventories.tolist()[0] + b2.inventories.tolist()[0]) / 2 )
  
  
  # 净资产收益率 税后净利润(不含少数股东权益) / 平均所有者权益(不含少数股东权益)
  in1.n_income_attr_p.tolist()[0] / ((b1.total_hldr_eqy_exc_min_int.tolist()[0] + b2.total_hldr_eqy_exc_min_int.tolist()[0]) / 2)
  
}

token <- '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'





