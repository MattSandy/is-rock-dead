
theme_set(theme_fivethirtyeight()+
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  legend.title = element_blank(),
                  strip.background = element_blank()))


albums <- read_csv("albums.csv")
tags <- read_csv("tags.csv")
lastfm_tags <- read_csv("lastfm_tags.csv")
tags_reclass <- read_csv("tags_reclass.csv")

df <- albums %>% 
  left_join(lastfm_tags, by = "artist") %>% 
  left_join(tags_reclass, by = c("name" = "tag")) %>% 
  rename(tag = name) %>% 
  mutate(year = year(date)) %>% 
  filter(!(artist == "Domingo" & tag == "hip-hop"))

top_tags <- df %>% 
  select(tag , category) %>% 
  group_by(tag, category) %>% 
  tally()



# Charts ------------------------------------------------------------------

# df %>% 
#   filter(category %in% c("rock", "hip-hop")) %>% 
#   group_by(category, year) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   ggplot(aes(x = year, y = n, col = category)) + 
#   geom_line() 






#path <- "/Users/taras/R/taraskaduk/content/posts/2019-10-rock-is-dead/"
path <- ""

draw_chart <- function(data, yaxis, title) {
  ggplot(data, aes_string(x = "year", y = yaxis, col = "tag")) + 
    geom_line(size = 1) +
    scale_color_brewer(palette = "Set1", type = "qual") + 
    scale_x_continuous(breaks = c(1963,  1980, 2000, 2018)) +
    labs(
      title = title,
      subtitle = "Years 1963 - 2018",
      caption = "Sources: Billboard 200 charts, Last FM
    See full methodology and code at taraskaduk.com
    
    taraskaduk.com | @taraskaduk"
    )
}

df %>% 
  filter(tag %in% c("rock", "hip-hop")) %>% 
  group_by(tag, year) %>% 
  tally() %>% 
  ungroup() %>% 
  draw_chart(yaxis = "n", title = "Count of albums appearing in weekly Billboard 200 charts")
ggsave(paste0(path,"1.png"))



df %>% 
  filter(tag %in% c("rock", "hip-hop")) %>% 
  select(tag, year, artist) %>% 
  distinct() %>% 
  group_by(tag, year) %>% 
  tally() %>% 
  ungroup() %>% 
  draw_chart(yaxis = "n", title = "Count of distict artists appearing in Billboard 200 every year")
ggsave(paste0(path,"2.png"))

df %>% 
  filter(tag %in% c("rock", "hip-hop")) %>% 
  select(tag, year, artist, age, type) %>% 
  distinct() %>% 
  group_by(tag, year, type) %>% 
  summarise(age = median(age)) %>% 
  ungroup() %>% 
  draw_chart(yaxis = "age", title = "Median age of artists and groups in Billboard 200") +
  facet_wrap(~type, scales = "free")
ggsave(paste0(path,"3.png"))

df %>% 
  filter(tag %in% c("rock", "hip-hop")) %>% 
  group_by(tag, year) %>% 
  tally() %>% 
  ungroup() %>% 
ggplot(aes(x = year, y = n, col = tag)) + 
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1", type = "qual") + 
  #labs(title = "Is Rock Dying?") +
  #scale_x_continuous(breaks = c(1963,  1980, 2000, 2018)) +
  theme(rect = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        axis.text = element_blank(),
        axis.title=element_blank(),
        panel.grid.major = element_blank(),
        # legend.position="top",
        # legend.key.size = unit(1, "cm"),
        # legend.text=element_text(size=14),
        legend.position = "none")
ggsave(paste0(path,"cover.png"), width = 100, height = 75, units = "mm")

# df %>% 
#   filter(tag %in% c("rock", "hip-hop")) %>% 
#   select(tag, year, artist,type, age) %>% 
#   group_by(artist) %>% 
#   mutate(filt = min(year) == year) %>%
#   ungroup() %>% 
#   filter(filt == 1) %>% 
#   distinct() %>% 
#   group_by(tag, year, type) %>% 
#   summarise(age = mean(age)) %>% 
#   ungroup() %>% 
#   draw_chart(yaxis = "age", title = "Mean age upon first entrance") +
#   facet_wrap(~type, scales = "free")


# df %>% 
#   filter(tag %in% c("rock", "hip-hop")) %>% 
#   filter(rank == 1) %>% 
#   select(tag, year, artist) %>% 
#   distinct() %>% 
#   group_by(tag, year) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   
#   ggplot(aes(x = year, y = n, col = tag)) + 
#   geom_line() 

      