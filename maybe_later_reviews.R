google_reviews_cuisine <- function(tbl) {
  
  tbl |> 
    mutate(restaurant = str_replace_all(restaurant, "Passage thru'", "Passage Thru"),
           restaurant = str_replace_all(restaurant, "Gravybaby", "GravyBaby"))  |>
    mutate(
      cuisine = case_when(
        str_detect(restaurant, "Thai|Bangkok|BKK|Baan") ~ "Thai",
        str_detect(restaurant, "Arab|Turkish|Al-|Halab|Egyptian") ~ "Levantine",
        str_detect(restaurant, "Oliver Gourmet") ~ "Seafood",
        str_detect(
          restaurant,
          "Italia|Olive|Ristorante|Osteria|Prego|Pizza|Trattoria|Nero Nero|Positano|a'Roma|Vin's Restaurant|Michelangelo's"
        ) ~ "Italian",
        str_detect(
          restaurant,
          "German|French|Chez|Marta's|Tapas|Bistro à Table|Iberico|El Toro Loco|Petit|Mercat|Dominic"
        ) ~ "European",
        str_detect(restaurant, "Whisky") ~ "Bar",
        str_detect(
          restaurant,
          "Chinese|Taiwan|[\\p{Han}]|Nam Heong|Dynasty|Putien|Hoi|Gold Dragon|Han Room|Village Duck|Village Roast Duck|Unique|Steamboat|Ah |Roast Pork|Hokkien Mee|Dim Sum|Lai Po Heen|Yun House|Chynna|Li Yen|Shanghai|Madam Kwan|Lai Foong|Little Penang Kafé|Goon Wah|Shang Palace|YEN"
        ) ~ "Chinese",
        str_detect(
          restaurant,
          "Nonya|Baba|Mum's Place|Chuup|Old China Cafe|Nyonya"
        ) ~ "Malaysian",
        str_detect(
          restaurant,
          "Grill|Steakhouse|Grub|Steak|Brasserie|steakhouse|BBQ|Rock Salt Restaurant|BBP|Churrascaria"
        ) ~ "Grill",
        str_detect(
          restaurant,
          "Japan|Edo|Sushi|Kampachi|Iketeru|Menya|Nobu|Omakase|TOKYO|Tokyo|Tonkatsu|Udon|Soba|Ichibanya|Maruhi|Omulab|Zipangu"
        ) ~ "Japanese",
        str_detect(
          restaurant,
          "Malay|Rasa|Cik|Dapur|Rebung|Zakhir|Village Park|ADU|Warisan|Nasi|Dancing Fish|Sarang"
        ) ~ "Malay",
        str_detect(
          restaurant,
          "Kerala|India|Lanka|Malabar|Curry|Raju|Bhavan|Thosai|Tandoor|Bombay|Gem Restaurant|Nasi Kandar|NADODI|Qureshi|RSMY|Sangeetha|Nirwana|Asian Rice Pot|Masala|Annalakshmi|Aliyaa|MTR"
        ) ~ "South Asian",
        str_detect(restaurant, "Latino|Mexican") ~ "Latino",
        str_detect(
          restaurant,
          "Filipino|Bali|Vietnam|GOODDAM|Korea|Sao Nam|Naughty Nuri's"
        ) ~ "Other Asian",
        str_detect(
          restaurant,
          "Bistro|Deli|Secret Recipe|Tujo|Wild Sheep|Antipodean|Coffee|One Half|Blue Room|Awesome Canteen|Botanica|Chili's|Don's Diner|GravyBaby|Hornbill|Huck's|myBurgerLab|Naj & Belle|Tony Roma's|Louisiana|Cor Blimey|4Fingers|Bacon And Balls|POP PIZZA|After Black|Define:Food|Ashley's by Living Food|March Azalea Kitchen|Mighty Monster IPC|Organic Express|Sköhns Canteen|Table9|Rabbit Hole|Williams Corner|Tiki Taka"
        ) ~ "Casual Western",
        str_detect(restaurant, "Cafe|Grind|Café|Tujo|cafe") ~ "Cafe",
        str_detect(restaurant, "DC Restaurant|ZENZERO") ~ "Other Upmarket",
        str_detect(restaurant, "Seafood|Shell Out") ~ "Seafood",
        str_detect(
          restaurant,
          "Lounge|Bar|Decanter|Knowhere Bangsar|The Locker & Loft|Tom, Dick & Harry's|WET|The Enclave|Out of Africa"
        ) ~ "Bar",
        str_detect(restaurant, "Restoran|Restaurant") ~ "Chinese",
        TRUE ~ "Upmarket"
      )) 
}


google_reviews |> 
  group_by(restaurant) |> 
  summarise(review_count = n(),
            rating = mean(rating)) |> 
  ggplot(aes(x = review_count, 
             y = rating)) + 
  geom_point(alpha = .15) + 
  scale_x_log10(breaks = c(0, 10, 30, 100, 300, 1000), 
                labels = comma) + 
  labs(title = "Distribution of Google review counts and ratings per restaurant in Malaysia", 
       subtitle = "The limit for scraping reviews from Google Maps is around 300 per restaurant", 
       x = "Number of reviews", 
       y = "Mean rating")  +
  
  trip_reviews |> 
  group_by(restaurant) |> 
  summarise(reviews = n(), 
            rating = mean(rating)) |> 
  ggplot(aes(x = reviews, y = rating)) + 
  geom_point(alpha = .15) + 
  scale_x_log10(breaks = c(0, 10, 30, 100, 300, 1000), 
                labels = comma) + 
  labs(title = "Distribution of Trip Advisor review counts and ratings per restaurant in Malaysia", 
       subtitle = "The limit for scraping reviews from Google Maps is around 300 per restaurant", 
       x = "Number of reviews", 
       y = "Mean rating")

google_reviews_with_cuisine |> 
  filter(rating == 5 & str_detect(review, "free|FREE") & 
           !str_detect(review, "dairy-free|hassle free|butter free|Pork free|pork free|freeflow|Freeflow|free flow|sugar free|sugar-free|sugarfreefree delivery|Delivery was free|freez|free refill|Free refill|Fuss free|fuss free|free WiFI|Free WiFi|free wifi|Free wifi|noise free|noise-free|Noise free|Noise-free|guilt free|Guilt free|Smoker free|smoke free|Smoke free")) |> 
  sample_n(10) |> 
  pull(review)
glimpse()

google_kl_cuisine |> 
  mutate(count = 1) |> 
  group_by(cuisine) |> 
  summarise(as_tibble_row(
    quantile(rating), 
    .name_repair = \(x) paste0('q', parse_number(x))), 
    .groups = "drop") |> 
  rowwise() |> 
  mutate(sort = mean(c(q25, q50))) |> 
  arrange(desc(sort))