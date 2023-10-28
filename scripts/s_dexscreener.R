library(tidyverse)

# track a pair as in DexScreener
cro_add_token(address = "0xbf62c67eA509E86F07c8c69d0286C0636C50270b", id = "vvswcro")
cro_add_token(address = "0x2D03bECE6747ADC00E1a131BBA1469C15fD11e03", id = "vvs")
cro_add_token(address = "0x5C7F8A570d578ED84E63fdFA7b1eE72dEae1AE23", id = "wcro")

cro_token("wcro")
cro_token("vvswcro")
cro_token("vvs")


lp_token <- cro_token("vvswcro")
token_1 <- cro_token("wcro")
token_2 <- cro_token("vvs")

balance_1 <- cro_account_crc_balance(account = lp_token, contract = token_1)
balance_2 <- cro_account_crc_balance(account = lp_token, contract = token_2)

last_block <- cro_block_number()$blockNumber - 50000
tx_1 <- cro_account_crc_tx(account = lp_token, contract = token_1, start_block = last_block)
tx_2 <- cro_account_crc_tx(account = lp_token, contract = token_2, start_block = last_block)


tx_data <- 
bind_rows(
  tx_1 %>% 
    select(timeStamp, from, to, value) %>%
    mutate(symbol = "wcro"),
  tx_2 %>% 
    select(timeStamp, from, to, value) %>%
    mutate(symbol = "vvs")
  ) %>%
  arrange(desc(timeStamp)) %>%
  mutate(
    side = case_when(
      from == lp_token ~ "BUY",
      to == lp_token ~ "SELL",
    ),
    value = ifelse(side == "BUY", -value, value)
  )

last_balance_1 <- balance_1$balance
last_balance_2 <- balance_2$balance
df <- tx_data
df$balance_1 <- 0
df$balance_2 <- 0
for(i in 1:nrow(df)){
  if(df[i,]$symbol == "vvs"){
    last_balance_2 <- last_balance_2 + df[i,]$value
    df$balance_2[i] <-  last_balance_2
    df$balance_1[i] <-  last_balance_1
  } else {
    last_balance_1 <- last_balance_1 + df[i,]$value
    df$balance_1[i] <-  last_balance_1
    df$balance_2[i] <-  last_balance_2
  }
}
df$price12 <- df$balance_1/df$balance_2
df$price21 <- df$balance_2/df$balance_1

ggplot(df)+
  geom_line(aes(timeStamp, price21))

ggplot(df)+
  geom_line(aes(timeStamp, price12))

df %>%
  group_by(from)%>%
  summarise(n = n()) %>%
  filter(n < 5) %>%
  arrange(desc(n))

address <- "0x72efd454bbfeca458e909b99faacbc116880b54d"
address <- "0x814920d1b8007207db6cb5a2dd92bf0b082bdba1"
address <- "0x54b083ca9fd30ae83c47cdd3a59390f00a90e9cc"


balance_address <- cro_account_crc_balance(account = address, contract = token_2)

balance_address$balance

df %>%
  filter(from == address | to == address) %>%
  filter(symbol == "vvs") %>%
  filter(side == "SELL")

df %>%
  filter(from == address | to == address) %>%
  filter(symbol == "vvs") %>%
  arrange(timeStamp) %>%
  ggplot()+
  geom_line(aes(timeStamp, balance_address$balance + cumsum(-value)))
















