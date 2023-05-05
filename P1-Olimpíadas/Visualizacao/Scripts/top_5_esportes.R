## packages
pacman::p_load(tidyverse, # manipulação de variáveis e ggplot2
               ggpattern, # preencher as barras com imagens
               ggtext,    # renderização de texto em HTML
               ggview)    # auxilio na visualização do plot




## dados
ranking = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/ranking.csv')
ranking = ranking |>
  mutate(posicao = c(3, 2, 4, 1, 5)) |> # ordenando as barras
  mutate(Sport = case_when(Sport=='Basketball'~'Basquete',
                           Sport=='Football'~'Futebol',
                           Sport=='Volleyball'~'Voleibol',
                           Sport=='Beach Volleyball' ~ 'Voleibol de praia',
                           Sport=='Judo' ~ 'Judô',
                           TRUE ~ Sport))




medalhas = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/medalhas.csv')
medalhas = medalhas |>
  left_join(ranking, by='Sport') |>
  mutate(ouro = case_when(Medal.x == 'Gold' ~ as.character(X0), TRUE ~ ''),
         prata = case_when(Medal.x == 'Silver' ~ as.character(X0), TRUE ~ ''),
         bronze = case_when(Medal.x == 'Bronze' ~ as.character(X0), TRUE ~ '')) |>
  mutate(med.label = case_when(ouro != '' ~ paste0('<span style="color:#FFE568;font-family:aw6;">\uf5a2', ouro, '</span>'),
                               prata != '' ~ paste0('<span style="color:#CCCCCC;font-family:aw6;">\uf5a2', prata, '</span>'),
                               bronze != '' ~ paste0('<span style="color:#C1A87D;font-family:aw6;">\uf5a2', bronze, '</span>'))) |>
  mutate(Sport = case_when(Sport=='Basketball'~'Basquete',
                           Sport=='Football'~'Futebol',
                           Sport=='Volleyball'~'Voleibol',
                           Sport=='Beach Volleyball' ~ 'Voleibol de praia',
                           Sport=='Judo' ~ 'Judô',
                           TRUE ~ Sport)) |>
  group_by(Sport) |>
  mutate(med.label = paste(med.label, collapse=' '))



## fontes de texto
sysfonts::font_add('aw6', 'fontes/Font Awesome 6 Free-Solid-900.otf') # para as medalhas
sysfonts::font_add_google("Quicksand","quicksand")                    # fonte padrao
showtext::showtext_auto()                                             # renderização de texto
showtext::showtext_opts(dpi=150)                                      # renderização de imagem





## pegando as imagens do github 
links = paste0('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Visualizacao/Imagens/', 
               c('basquete.jpg', 'futebol.jpeg', 'judo.jpeg', 'voleibol.jpg', 'voleibol_de_praia.jpg'))





## plot
ggplot(data = ranking, 
       aes(x = reorder(Sport, posicao), y = Medal, fill = Sport)) +
  geom_bar_pattern(aes(pattern_angle = Sport, 
                       pattern_filename = Sport), 
                   pattern = 'image',
                   pattern_type = 'expand',
                   stat = 'identity',
                   linewidth = 0
  ) +
  scale_pattern_filename_discrete(choices = links) +
  geom_richtext(aes(x=reorder(Sport, posicao), y=Medal+.5,
                    label=paste0("<span style='font-size:75pt;'><b>", toupper(str_replace(Sport, ' ', '<br>')), "<br></span>",
                                 "<span style='font-size:70pt;'><b>", Medal, '</b> medalhas')),
                family='quicksand',
                color='#F5F3F4',
                label.color=NA,
                lineheight=3.3,
                fill=NA,
                vjust=0) +
  geom_richtext(data = medalhas,
                aes(x=Sport, y=Medal.y+.1, label=med.label),
                family='aw6',
                size=19,
                label.color=NA,
                fill=NA,
                vjust=0,
                hjust=.5) +
  scale_y_continuous(limits=c(0,8.5)) +
  labs(title='O PÓDIO DELAS',
       subtitle='Os **5** esportes com mais medalhas conquistadas por brasileiras.') +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill='#BC4B51', color='#BC4B51'),
        plot.title = element_text(family='quicksand',
                                  face='bold',
                                  size=120,
                                  color='#F5F3F4',
                                  hjust=.105),
        plot.subtitle = element_markdown(family='quicksand',
                                         size=75,
                                         color='#F5F3F4',
                                         hjust=.5),
        plot.margin = margin(t=35, b=-40, l=10, r=11),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())





## salvando
ggview(units='px', height=4500, width=5500)
ggsave(units='px', height=4500, width=5500, filename='top5_esportes.png')
